# High level functions for applying gradients as fills to grobs
grid.gradientFill <- function(path, label = NULL, gradient = NULL,
                              alpha = 1, group = TRUE, grep = FALSE) {
    if (is.null(label) & is.null(gradient)) {
        stop("At least one of 'label' or 'gradient' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.gradientFill")
        gradientFill(label, gradient)
    } else if (is.null(gradient)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        gradientFill(label, gradient)
        gradient <- NULL # use the ref from now on
    }

    if (any(grep)) {
        grobApply(path, function(path) {
            grid.set(path, gradientFillGrob(grid.get(path), label = label,
                                            gradient = gradient, alpha = alpha,
                                            group = group))
        }, grep = grep)
    } else {
        grid.set(path,
                 gradientFillGrob(grid.get(path), label = label,
                                  gradient = gradient, alpha = alpha,
                                  group = group))
    }
}

gradientFillGrob <- function(x, label = NULL, gradient = NULL,
                             alpha = 1, group = TRUE) {
    if (is.null(label) & is.null(gradient)) {
        stop("At least one of 'label' or 'gradient' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.gradientFill")
        gradientFill(label, gradient)
    } else if (is.null(gradient)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        gradientFill(label, gradient)
    }

    x$label <- label
    label <- getLabelID(label)
    # Allowing fill-opacity to be set by a garnish because
    # grid only knows about a colour and its opacity. If we use a
    # reference instead of a then nothing is known about the opacity.
    # We want to ensure that we can still set it, so use the garnish
    # to overwrite it.
    x <- garnishGrob(x, fill = paste0("url(#", label, ")"),
                     "fill-opacity" = alpha, group = group)
    class(x) <- unique(c("gradientFilled.grob", "referring.grob", class(x)))
    x
}

linearGradient <- function(gradientUnits = c("bbox", "coords"),
                           x = unit(c(0, 1), "npc"),
                           y = unit(c(0, 1), "npc"),
                           default.units = "npc",
                           spreadMethod = c("pad", "reflect", "repeat"),
                           stops = NULL) {
    gradientUnits <- match.arg(gradientUnits)
    spreadMethod <- match.arg(spreadMethod)
    if (is.null(stops))
        stops <- list()
    
    # Convert gradientUnits to SVG values
    gradientUnits <- switch(gradientUnits,
                            bbox = "objectBoundingBox",
                            coords = "userSpaceOnUse")

    # Need to get npc-like values from units
    if (gradientUnits == "objectBoundingBox") {
        if (is.unit(x))
            x <- convertX(x, "npc", valueOnly = TRUE)
        if (is.unit(y))
            y <- convertY(y, "npc", valueOnly = TRUE)
    } else {
        if (! is.unit(x))
            x <- unit(x, default.units)
        if (! is.unit(y))
            y <- unit(y, default.units)
    }

    grad <- list(element = "linearGradient",
                 gradientUnits = gradientUnits,
                 x1 = x[1], x2 = x[2],
                 y1 = y[1], y2 = y[2],
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
    
    # Convert gradientUnits to SVG values
    gradientUnits <- switch(gradientUnits,
                            bbox = "objectBoundingBox",
                            coords = "userSpaceOnUse")

    # Need to get npc-like values from units
    if (gradientUnits == "objectBoundingBox") {
        if (is.unit(x))
            x <- convertX(x, "npc", valueOnly = TRUE)
        if (is.unit(y))
            y <- convertY(y, "npc", valueOnly = TRUE)
        if (is.unit(r)) {
            rw <- convertWidth(r, "npc", valueOnly = TRUE)
            rh <- convertHeight(r, "npc", valueOnly = TRUE)
            r <- pmin(rw, rh)
        }
        if (is.unit(fx))
            fx <- convertX(fx, "npc", valueOnly = TRUE)
        if (is.unit(fy))
            fy <- convertY(fy, "npc", valueOnly = TRUE)
    } else {
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
    x <- list(offset = offset,
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

gradientFill <- function(label, gradient) {
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
        def$x1 <- round(cx(def$x1, dev), 2)
        def$x2 <- round(cx(def$x2, dev), 2)
        def$y1 <- round(cy(def$y1, dev), 2)
        def$y2 <- round(cy(def$y2, dev), 2)
    }

    gradient <- newXMLNode("linearGradient",
        parent = svgDevParent(svgdev),
        attrs = list(id = prefixName(def$id),
                     x1 = def$x1, x2 = def$x2,
                     y1 = def$y1, y2 = def$y2,
                     gradientUnits = def$gradientUnits,
                     spreadMethod = def$spreadMethod))

    svgDevChangeParent(gradient, svgdev)
}

svgRadialGradient <- function(def, dev) {
    svgdev <- dev@dev

    # Convert grid coords to SVG coords if we are using coordinates
    # rather than the bounding box of the referring object
    if (def$gradientUnits == "userSpaceOnUse") {
        def$cx <- round(cx(def$cx, dev), 2)
        def$cy <- round(cy(def$cy, dev), 2)
        def$r <- round(cd(def$r, dev), 2)
        def$fx <- round(cx(def$fx, dev), 2)
        def$fy <- round(cy(def$fy, dev), 2)
    }

    gradient <- newXMLNode("radialGradient",
        parent = svgDevParent(svgdev),
        attrs = list(id = prefixName(def$id),
                     cx = def$cx, cy = def$cy, r = def$r,
                     fx = def$fx, fy = def$fy,
                     gradientUnits = def$gradientUnits,
                     spreadMethod = def$spreadMethod))

    svgDevChangeParent(gradient, svgdev)
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

