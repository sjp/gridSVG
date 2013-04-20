# High level functions for applying gradients as fills to grobs
grid.gradientFill <- function(path, gradient = NULL, label = NULL,
                              alpha = 1, group = TRUE, redraw = FALSE,
                              strict = FALSE, grep = FALSE, global = FALSE) {
    if (is.null(gradient) & is.null(label)) {
        stop("At least one of 'gradient' or 'label' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.gradientFill")
        registerGradientFill(label, gradient)
    } else if (is.null(gradient)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerGradientFill(label, gradient)
        gradient <- NULL # use the ref from now on
    }

    grobApply(path, function(path) {
        grid.set(path, gradientFillGrob(grid.get(path), gradient = gradient,
                                        label = label, alpha = alpha,
                                        group = group),
                 redraw = redraw)
    }, strict = strict, grep = grep, global = global)

    invisible()
}

gradientFillGrob <- function(x, gradient = NULL, label = NULL,
                             alpha = 1, group = TRUE) {
    if (is.null(gradient) & is.null(label)) {
        stop("At least one of 'gradient' or 'label' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.gradientFill")
        registerGradientFill(label, gradient)
    } else if (is.null(gradient)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerGradientFill(label, gradient)
    }

    x$referenceLabel <- c(x$referenceLabel, label)
    x$gradientFillLabel <- label
    x$gradientFillAlpha <- alpha
    x$gradientFillGroup <- group
    class(x) <- unique(c("gradientFilled.grob", class(x)))
    x
}

linearGradient <- function(gradientUnits = c("bbox", "coords"),
                           x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                           y0 = unit(0, "npc"), y1 = unit(1, "npc"),
                           default.units = "npc",
                           spreadMethod = c("pad", "reflect", "repeat"),
                           stops = NULL) {
    gradientUnits <- match.arg(gradientUnits)
    spreadMethod <- match.arg(spreadMethod)
    if (is.null(stops))
        stops <- list()
    
    if (! is.unit(x0))
        x0 <- unit(x0, default.units)
    if (! is.unit(x1))
        x1 <- unit(x1, default.units)
    if (! is.unit(y0))
        y0 <- unit(y0, default.units)
    if (! is.unit(y1))
        y1 <- unit(y1, default.units)

    # Convert gradientUnits to SVG values
    gradientUnits <- switch(gradientUnits,
                            bbox = "objectBoundingBox",
                            coords = "userSpaceOnUse")

    # Need to get npc-like values from units
    if (gradientUnits == "objectBoundingBox") {
        # Convert to npc 
        x0 <- convertX(x0, "npc", valueOnly = TRUE)
        x1 <- convertX(x1, "npc", valueOnly = TRUE)
        y0 <- convertY(y0, "npc", valueOnly = TRUE)
        y1 <- convertY(y1, "npc", valueOnly = TRUE)

        # Clamp to [0,1]
        x0 <- if (x0 < 0) 0 else if (x0 > 1) 1 else x0
        x1 <- if (x1 < 0) 0 else if (x1 > 1) 1 else x1
        y0 <- if (y0 < 0) 0 else if (y0 > 1) 1 else y0
        y1 <- if (y1 < 0) 0 else if (y1 > 1) 1 else y1
    }

    grad <- list(element = "linearGradient",
                 gradientUnits = gradientUnits,
                 x1 = x0, x2 = x1,
                 y1 = y0, y2 = y1,
                 spreadMethod = spreadMethod,
                 children = stops)
    class(grad) <- c("linear.gradient", "gradient")
    grad
}

radialGradient <- function(gradientUnits = c("bbox", "coords"),
                           x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                           r = unit(0.5, "npc"),
                           fx = unit(0.5, "npc"), fy = unit(0.5, "npc"),
                           default.units = "npc",
                           spreadMethod = c("pad", "reflect", "repeat"),
                           stops = NULL) {
    gradientUnits <- match.arg(gradientUnits)
    spreadMethod <- match.arg(spreadMethod)
    if (is.null(stops))
        stops <- list()
    
    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(r))
        r <- unit(r, default.units)
    if (! is.unit(fx))
        fx <- unit(fx, default.units)
    if (! is.unit(fy))
        fy <- unit(fy, default.units)

    # Convert gradientUnits to SVG values
    gradientUnits <- switch(gradientUnits,
                            bbox = "objectBoundingBox",
                            coords = "userSpaceOnUse")

    # Need to get npc-like values from units
    if (gradientUnits == "objectBoundingBox") {
        x <- convertX(x, "npc", valueOnly = TRUE)
        y <- convertY(y, "npc", valueOnly = TRUE)

        rw <- convertWidth(r, "npc", valueOnly = TRUE)
        rh <- convertHeight(r, "npc", valueOnly = TRUE)
        r <- pmin(rw, rh)

        fx <- convertX(fx, "npc", valueOnly = TRUE)
        fy <- convertY(fy, "npc", valueOnly = TRUE)

        # Do not clamp 'r' to be less than one, we could want a radius to
        # fill from a corner in a square, in which case 'r' > 1 is necesary.
        x <- if (x < 0) 0 else if (x > 1) 1 else x
        y <- if (y < 0) 0 else if (y > 1) 1 else y
        fx <- if (fx < 0) 0 else if (fx > 1) 1 else fx
        fy <- if (fy < 0) 0 else if (fy > 1) 1 else fy
        r <- if (r < 0) 0 else r
    }

    grad <- list(element = "radialGradient",
                 gradientUnits = gradientUnits,
                 cx = x, cy = y, r = r,
                 fx = fx, fy = fy,
                 spreadMethod = spreadMethod,
                 children = stops)
    class(grad) <- c("radial.gradient", "gradient")
    grad
}

"[.gradient" <- function(x, index, ...) {
    x$children <- x$children[index]
    x
}

"[[.gradient" <- function(x, index, ...) {
    x$children[[index]]
}

"[<-.gradient" <- function(x, index, ..., value) {
    x$children[index] <- value
    x
}

"[[<-.gradient" <- function(x, index, ..., value) {
    if (! inherits(value, "gradient.stop"))
        stop("Invalid value to assign")
    x$children[[index]] <- value
    x
}

print.gradient <- function(x, ...) {
    prln <- function(label, value) {
        cat(sprintf(paste0(label, ": %s\n"), value))
    }
    prln("Type", x$element)
    n <- length(x$children)
    prln("Number of stops", if (! n) "none" else n)
    if (! n)
        return(invisible(x))
    cat("\n")
    prln("Gradient stops", "")
    for (i in 1:n) {
        cat("  ")
        print(x[[i]])
    }
    invisible(x)
}

print.gradient.stop <- function(x, ...) {
    cat(sprintf("Offset: %s, Colour: %s, Opacity: %s\n",
                x$offset, x$`stop-color`, x$`stop-opacity`))
    invisible(x)
}

addGradientStop <- function(gradient, gradientStop, after = NA) {
    if (! inherits(gradient, "gradient"))
        stop("'gradient' is not a 'gradient.stop' object")
    if (! inherits(gradientStop, "gradient.stop"))
        stop("'gradientStop' is not a 'gradient.stop' object")
    # Assume last
    if (is.na(after))
        after <- length(gradient$children)
    gradient[[after + 1]] <- gradientStop
    gradient
}

gradientStop <- function(offset = 0, col) {
    rgba <- col2rgb(col, alpha = TRUE)[, 1]
    x <- list(offset = round(offset, 2),
              "stop-color" = devColToSVG(col),
              "stop-opacity" = devColAlphaToSVG(rgba[4]))
    class(x) <- "gradient.stop"
    x
}

flattenLinearGradient <- function(gradient) {
    # Flatten all locations here
    if (gradient$gradientUnits == "userSpaceOnUse") {
        gradient$x1 <- convertX(gradient$x1, "inches")
        gradient$x2 <- convertX(gradient$x2, "inches")
        gradient$y1 <- convertY(gradient$y1, "inches")
        gradient$y2 <- convertY(gradient$y2, "inches")
    }
    gradient
}

flattenRadialGradient <- function(gradient) {
    # Flatten all locations here
    if (gradient$gradientUnits == "userSpaceOnUse") {
        gradient$cx <- convertX(gradient$cx, "inches")
        gradient$cy <- convertX(gradient$cy, "inches")
        gradient$r <- dToInches(gradient$r, NULL)
        gradient$fx <- convertX(gradient$fx, "inches")
        gradient$fy <- convertY(gradient$fy, "inches")
    }
    gradient
}

registerGradientFill <- function(label, gradient) {
    checkExistingDefinition(label)
    if (! length(gradient$children))
        stop("No gradient stops exist for this gradient.")
    
    # Flattening all locations
    gradient <-
        if (inherits(gradient, "radial.gradient"))
            flattenRadialGradient(gradient)
        else
            flattenLinearGradient(gradient)

    gradient$label <- label
    gradient$id <- getID(label, "ref")
    class(gradient) <- "gradientDef"

    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)
    refDefinitions[[label]] <- gradient
    assign("refDefinitions", refDefinitions, envir = .gridSVGEnv)
    assign("refUsageTable",
           rbind(get("refUsageTable", envir = .gridSVGEnv),
                 data.frame(label = label, used = FALSE,
                            stringsAsFactors = FALSE)),
           envir = .gridSVGEnv)

    # Return NULL invisibly because we don't actually care what the
    # definition looks like until gridSVG tries to draw it. 
    invisible()
}

svgLinearGradient <- function(def, dev) {
    svgdev <- dev@dev

    # Convert grid coords to SVG coords if we are using coordinates
    # rather than the bounding box of the referring object
    if (def$gradientUnits == "userSpaceOnUse") {
        def$x1 <- cx(def$x1, dev)
        def$x2 <- cx(def$x2, dev)
        def$y1 <- cy(def$y1, dev)
        def$y2 <- cy(def$y2, dev)
    }

    gradient <- newXMLNode("linearGradient",
        parent = svgDevParent(svgdev),
        attrs = list(id = prefixName(def$id),
                     x1 = round(def$x1, 2), x2 = round(def$x2, 2),
                     y1 = round(def$y1, 2), y2 = round(def$y2, 2),
                     gradientUnits = def$gradientUnits,
                     spreadMethod = def$spreadMethod))

    svgDevChangeParent(gradient, svgdev)
}

svgRadialGradient <- function(def, dev) {
    svgdev <- dev@dev

    # Convert grid coords to SVG coords if we are using coordinates
    # rather than the bounding box of the referring object
    if (def$gradientUnits == "userSpaceOnUse") {
        def$cx <- cx(def$cx, dev)
        def$cy <- cy(def$cy, dev)
        def$r <- cd(def$r, dev)
        def$fx <- cx(def$fx, dev)
        def$fy <- cy(def$fy, dev)
    }

    gradient <- newXMLNode("radialGradient",
        parent = svgDevParent(svgdev),
        attrs = list(id = prefixName(def$id),
                     cx = round(def$cx, 2), cy = round(def$cy, 2),
                     r = round(def$r, 2),
                     fx = round(def$fx, 2), fy = round(def$fy, 2),
                     gradientUnits = def$gradientUnits,
                     spreadMethod = def$spreadMethod))

    svgDevChangeParent(gradient, svgdev)
}

primToDev.gradientFilled.grob <- function(x, dev) {
    setLabelUsed(x$referenceLabel)
    label <- getLabelID(x$gradientFillLabel)
    # Allowing fill-opacity to be set by a garnish because
    # grid only knows about a colour and its opacity. If we use a
    # reference instead of a then nothing is known about the opacity.
    # We want to ensure that we can still set it, so use the garnish
    # to overwrite it.
    gf <- garnishGrob(x, fill = paste0("url(#", label, ")"),
                      "fill-opacity" = x$gradientFillAlpha,
                      group = x$gradientFillGroup)
    # Now need to remove all gradient fill appearances in the class list.
    # This is safe because repeated gradient filling just clobbers existing
    # attributes.
    cl <- class(gf)
    class(gf) <- cl[cl != "gradientFilled.grob"]
    primToDev(gf, dev)
}

drawDef.gradientDef <- function(def, dev) {
    svgdev <- dev@dev

    if (def$element == "linearGradient")
        svgLinearGradient(def, dev)
    else
        svgRadialGradient(def, dev)

    # Adding the gradient stops
    children <- def$children
    for (i in 1:length(children)) {
        stpattrs <- unclass(children[[i]])
        newXMLNode("stop", attrs = stpattrs, parent = svgDevParent(svgdev))
    }

    # Going back up from the stops to the parent of the gradient
    svgDevChangeParent(xmlParent(svgDevParent(svgdev)), svgdev)
}

