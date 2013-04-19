# High level functions for applying filters to grobs
grid.filter <- function(path, filter = NULL, label = NULL,
                        group = TRUE, grep = FALSE) {
    if (is.null(filter) & is.null(label)) {
        stop("At least one of 'filter' or 'label' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.filter")
        registerFilter(label, filter)
    } else if (is.null(filter)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerFilter(label, filter)
        filter <- NULL # use the ref from now on
    }

    if (any(grep)) {
        grobApply(path, function(path) {
            grid.set(path, filterGrob(grid.get(path), label = label,
                                      filter = filter, group = group))
        }, grep = grep)
    } else {
        grid.set(path,
                 filterGrob(grid.get(path), label = label,
                            filter = filter, group = group))
    }
}

filterGrob <- function(x, filter = NULL, label = NULL,
                       alpha = 1, group = TRUE) {
    if (is.null(filter) & is.null(label)) {
        stop("At least one of 'filter' or 'label' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.filter")
        registerFilter(label, filter)
    } else if (is.null(filter)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerFilter(label, filter)
    }

    x$label <- label
    label <- getLabelID(label)
    x <- garnishGrob(x, filter = paste0("url(#", label, ")"),
                     group = group)
    class(x) <- unique(c("filtered.grob", "referring.grob", class(x)))
    x
}

filterEffect <- function(filterUnits = c("coords", "bbox"),
                         x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                         width = unit(1, "npc"), height = unit(1, "npc"),
                         just = "centre", hjust = NULL, vjust = NULL,
                         default.units = "npc",
                         primitiveUnits = c("coords", "bbox"),
                         filterEffects = NULL) {
    filterUnits <- match.arg(filterUnits)
    primitiveUnits <- match.arg(primitiveUnits)
    if (is.null(filterEffects))
        filterEffects <- list()
    if (inherits(filterEffects, "filter.effect"))
        filterEffects <- list(filterEffects)
    
    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(width))
        width <- unit(width, default.units)
    if (! is.unit(height))
        height <- unit(height, default.units)

    # Convert filterUnits to SVG values
    filterUnits <- switch(filterUnits,
                          bbox = "objectBoundingBox",
                          coords = "userSpaceOnUse")
    primitiveUnits <- switch(primitiveUnits,
                             bbox = "objectBoundingBox",
                             coords = "userSpaceOnUse")

    # Need to get npc-like values from units
    if (filterUnits == "objectBoundingBox") {
        # Convert to npc 
        x <- convertX(x, "npc", valueOnly = TRUE)
        y <- convertY(y, "npc", valueOnly = TRUE)
        width <- convertWidth(width, "npc", valueOnly = TRUE)
        height <- convertHeight(height, "npc", valueOnly = TRUE)
    }

    filter <- list(filterUnits = filterUnits,
                   primitiveUnits = primitiveUnits,
                   x = x, y = y,
                   width = width, height = height,
                   just = just, hjust = hjust, vjust = vjust,
                   children = filterEffects)
    class(filter) <- "filter"
    filter
}

"[.filter" <- function(x, index, ...) {
    x$children <- x$children[index]
    x
}

"[[.filter" <- function(x, index, ...) {
    x$children[[index]]
}

"[<-.filter" <- function(x, index, ..., value) {
    x$children[index] <- value
    x
}

"[[<-.filter" <- function(x, index, ..., value) {
    if (! inherits(value, "filter.effect"))
        stop("Invalid value to assign")
    x$children[[index]] <- value
    x
}

addFilterEffect <- function(filter, filterEffect, after = NA) {
    if (! inherits(filter, "filter"))
        stop("'filter' is not an 'filter' object")
    if (! inherits(filterEffect, "filter.effect"))
        stop("'filterEffect' is not a 'filter.effect' object")
    # Assume last
    if (is.na(after))
        after <- length(filter$children)
    filter$children[[after + 1]] <- filterEffect
    filter
}

flatten <- function(x, coords) {
    UseMethod("flatten")
}

flatten.filter <- function(x, coords = TRUE) {
    loc <- leftbottom(x$x, x$y, x$width, x$height,
                      x$just, x$hjust, x$vjust, NULL)
    if (coords) {
        x$x <- loc$x
        x$y <- loc$y
        x$width <- convertWidth(x$width, "inches")
        x$height <- convertHeight(x$height, "inches")
    } else {
        x$x <- convertX(loc$x, "npc", valueOnly = TRUE)
        x$y <- convertY(loc$y, "npc", valueOnly = TRUE)
        x$width <- convertWidth(x$width, "npc", valueOnly = TRUE)
        x$height <- convertHeight(x$height, "npc", valueOnly = TRUE)
    }

    # Now flatten all children
    x$children <- lapply(x$children, flatten, x$primitiveUnits == "userSpaceOnUse")

    x 
}

registerFilter <- function(label, filter) {
    checkExistingDefinition(label)
    if (! length(filter$children))
        stop("No filter effects exist for this filter.")
    
    # Flattening all locations
    filter <- flatten(filter, filter$filterUnits == "userSpaceOnUse")

    filter$label <- label
    filter$id <- getID(label, "ref")
    class(filter) <- "filterDef"

    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)
    refDefinitions[[label]] <- filter
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

svgFilter <- function(def, dev) {
    svgdev <- dev@dev

    if (def$filterUnits == "userSpaceOnUse") {
        def$x <- cx(def$x, dev)
        def$y <- cy(def$y, dev)
        def$width <- cw(def$width, dev)
        def$height <- ch(def$height, dev)
    }

    filter <- newXMLNode("filter",
                         attrs = list(id = prefixName(def$id),
                                      x = round(def$x, 2),
                                      y = round(def$y, 2),
                                      width = round(def$width, 2),
                                      height = round(def$height, 2),
                                      filterUnits = def$filterUnits,
                                      primitiveUnits = def$primitiveUnits),
                         parent = svgDevParent(svgdev))
    svgDevChangeParent(filter, svgdev)
}

drawDef.filterDef <- function(def, dev) {
    svgdev <- dev@dev

    svgFilter(def, dev)

    # Adding the gradient stops
    children <- def$children
    for (i in 1:length(children)) {
        oldclass <- class(children[[i]])
        child <- cleanAttrs(children[[i]], c("just", "hjust", "vjust"))
        child <- compileUnits(child, dev)
        class(child) <- oldclass
        filterSVG(child, dev)
    }

    # Going back up from the filter to the parent of the filter
    svgDevChangeParent(xmlParent(svgDevParent(svgdev)), svgdev)
}

# Remove unnecessary attributes
cleanAttrs <- function(x, attrs = "") {
    ns <- names(x)
    rmInds <- which(ns %in% attrs)
    if (length(rmInds))
        x[-rmInds]
    else
        x
}

# All filter effects have these in common,
# compile the units to px to allow us to have more specific
# methods later
compileUnits <- function(x, dev) {
    x$x <- cx(x$x, dev)
    x$y <- cy(x$y, dev)
    x$width <- cw(x$width, dev)
    x$height <- ch(x$height, dev)
    x
}

# rounding all numerics to 2 dp
roundAttribs <- function(x) {
    lapply(x, function(a) {
        if (is.numeric(a))
            round(a, 2)
        else
            a
    })
}

filterSVG <- function(x, dev) {
    UseMethod("filterSVG")
}

##################
# Filter Effects #
##################

# Light sources

flatten.filter.effect <- function(x, coords = TRUE) {
    loc <- leftbottom(x$x, x$y, x$width, x$height,
                      x$just, x$hjust, x$vjust, NULL)
    if (coords) {
        x$x <- loc$x
        x$y <- loc$y
        x$width <- convertWidth(x$width, "inches")
        x$height <- convertHeight(x$height, "inches")
    } else {
        x$x <- convertX(loc$x, "npc", valueOnly = TRUE)
        x$y <- convertY(loc$y, "npc", valueOnly = TRUE)
        x$width <- convertWidth(x$width, "npc", valueOnly = TRUE)
        x$height <- convertHeight(x$height, "npc", valueOnly = TRUE)
    }
    x
}

fe <- function(..., x = unit(0.5, "npc"), y = unit(0.5, "npc"),
               width = unit(1, "npc"), height = unit(1, "npc"),
               just = "centre", hjust = NULL, vjust = NULL,
               default.units = "npc", result = NULL) {
    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(width))
        width <- unit(width, default.units)
    if (! is.unit(height))
        height <- unit(height, default.units)

    x <- list(x = x, y = y,
              width = width, height = height,
              just = just, hjust = hjust, vjust = vjust)
    if (! is.null(result) && nzchar(result))
        x$result <- result
    x <- c(x, list(...))
    class(x) <- "filter.effect"
    x
}

filterSVG.fe.distant.light <- function(x, dev) {
    svgdev <- dev@dev
    newXMLNode("feDistantLight",
               attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feDistantLight <- function(azimuth = 0, elevation = 0, ...) {
    x <- fe(azimuth = azimuth, elevation = elevation, ...)
    class(x) <- c("fe.distant.light", class(x))
    x
}

flatten.fe.point.light <- function(x, coords = TRUE) {
    if (coords) {
        x$z <- if (x$zdim == "x") convertX(x$z, "inches")
               else convertY(x$z, "inches")
    } else {
        x$z <- if (x$dzim == "x") convertX(x$z, "npc", valueOnly = TRUE)
               else convertY(x$z, "npc", valueOnly = TRUE)
    }

    NextMethod()
}

filterSVG.fe.point.light <- function(x, dev) {
    svgdev <- dev@dev
    x$z <- if (x$zdim == "x") cw(x$z, dev) else cy(x$z, dev)
    attrList <- cleanAttrs(x, "zdim")
    newXMLNode("fePointLight",
               attrs = roundAttribs(attrList),
               parent = svgDevParent(svgdev))
}

fePointLight <- function(z = unit(0, "npc"), default.units = "npc",
                         zdim = "x", ...) {
    if (! is.unit(z))
        z <- unit(z, default.units)
    x <- fe(z = z, zdim = zdim, default.units = default.units, ...)
    class(x) <- c("fe.point.light", class(x))
    x
}

flatten.fe.spot.light <- function(x, coords = TRUE) {
    if (coords) {
        x$z <- if (x$zdim == "x") convertX(x$z, "inches")
               else convertY(x$z, "inches")
        x$pointsAtZ <- if (x$zdim == "x") convertX(x$pointsAtZ, "inches")
                       else convertY(x$pointsAtZ, "inches")
        x$pointsAtX <- convertX(x$pointsAtX, "inches")
        x$pointsAtY <- convertY(x$pointsAtY, "inches")
    } else {
        x$z <- if (x$dzim == "x") convertX(x$z, "npc", valueOnly = TRUE)
               else convertY(x$z, "npc", valueOnly = TRUE)
        x$pointsAtZ <- if (x$zdim == "x") convertX(x$pointsAtZ, "npc", valueOnly = TRUE)
                       else convertY(x$pointsAtZ, "npc", valueOnly = TRUE)
        x$pointsAtX <- convertX(x$pointsAtX, "npc", valueOnly = TRUE)
        x$pointsAtY <- convertY(x$pointsAtY, "npc", valueOnly = TRUE)
    }

    NextMethod()
}

filterSVG.fe.spot.light <- function(x, dev) {
    svgdev <- dev@dev
    x$z <- if (x$zdim == "x") cw(x$z, dev) else cy(x$z, dev)
    x$pointsAtZ <- if (x$zdim == "x") cw(x$pointsAtZ, dev)
                   else cy(x$pointsAtZ, dev)
    x$pointsAtX <- cx(x$x, dev)
    x$pointsAtY <- cy(x$pointsAtY, dev)
    attrList <- cleanAttrs(x, "zdim")
    newXMLNode("feSpotLight",
               attrs = roundAttribs(attrList),
               parent = svgDevParent(svgdev))
}

feSpotLight <- function(x = unit(0, "npc"), y = unit(0, "npc"), z = unit(0, "npc"),
                        pointsAtX = unit(1, "npc"), pointsAtY = unit(1, "npc"), pointsAtZ = unit(0, "npc"),
                        zdim = "x", default.units = "npc",
                        specularExponent = 1, limitingConeAngle = NA, ...) {
    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(z))
        z <- unit(z, default.units)
    if (! is.unit(pointsAtX))
        pointsAtX <- unit(pointsAtX, default.units)
    if (! is.unit(pointsAtY))
        pointsAtY <- unit(pointsAtY, default.units)
    if (! is.unit(pointsAtZ))
        pointsAtZ <- unit(pointsAtZ, default.units)
    x <- fe(x = x, y = y, z = z,
            pointsAtX = pointsAtX, pointsAtY = pointsAtY, pointsAtZ = pointsAtZ,
            zdim = zdim, default.units = default.units,
            specularExponent = specularExponent, ...)
    if (! is.na(limitingConeAngle))
        x$limitingConeAngle <- limitingConeAngle
    class(x) <- c("fe.spot.light", class(x))
    x
}

filterSVG.fe.blend <- function(x, dev) {
    svgdev <- dev@dev
    newXMLNode("feBlend",
               attrs = roundAttribs(x), parent = svgDevParent(svgdev))
}

feBlend <- function(input1 = NA, input2 = NA,
                    mode = c("normal", "multiply", "screen", "darken", "lighten"),
                    ...) {
    x <- fe(mode = match.arg(mode), ...)
    if (! is.na(input1))
        x$`in` <- input1
    if (! is.na(input2))
        x$in2 <- input2
    class(x) <- c("fe.blend", class(x))
    x
}

filterSVG.fe.color.matrix <- function(x, dev) {
    svgdev <- dev@dev
    attrList <- x
    if (x$type == "luminanceToAlpha")
        attrList <- cleanAttrs(attrList, "values")
    if (x$type == "matrix") {
        rows <- apply(x$values, 1, function(x) {
            paste0(round(x, 2), collapse = " ")
        })
        x$values <- paste0(rows, collapse = " ")
    }

    newXMLNode("feColorMatrix",
               attrs = roundAttribs(attrList),
               parent = svgDevParent(svgdev))
}

feColorMatrix <- function(input = NA,
                          type = c("matrix", "saturate", "hueRotate", "luminanceToAlpha"),
                          values = NULL, ...) {
    # Checking validity of args
    if (type == "matrix" && (! is.matrix(values) || ! dim(values) == c(4, 5)))
        stop("'values' must be a 4x5 numeric matrix when 'type' is 'matrix'")
    if (type == "saturate" && ! is.numeric(values))
        stop("'values' must be a single element numeric vector for 'saturate'")
    if (type == "hueRotate" && ! is.numeric(values))
        stop("'values' must be a single element numeric vector for 'hueRotate'")
    if (type == "luminanceToAlpha" && ! is.null(values))
        stop("'values' must be NULL for the 'luminanceToAlpha' color matrix effect")
    
    # Clamp values to valid bounds
    if (type == "matrix")
        values <- matrix(pmax(0, pmin(1, values)), ncol = 5, nrow = 4)
    if (type == "saturate")
        values <- max(0, min(1, values))
    if (type == "hueRotate")
        values <- values %% 360

    x <- fe(type = match.arg(type), values = values, ...)
    if (! is.na(input))
        x$`in` <- input
    class(x) <- c("fe.color.matrix", class(x))
    x
}

filterSVG.fe.component.transfer <- function(x, dev) {
    svgdev <- dev@dev

    parentAttrs <- cleanAttrs(x, "transfers")
    children <- x$transfers

    cm <- newXMLNode("feColorMatrix",
                     attrs = roundAttribs(parentAttrs),
                     parent = svgDevParent(svgdev))

    if (! length(children))
        return()

    svgDevChangeParent(cm, svgdev)

    for (i in 1:length(children)) {
        child <- children[[i]]
        child$channel <- names(children)[i]
        filterSVG(child, dev)
    }

    svgDevChangeParent(xmlParent(cm), svgdev)
}

feComponentTransfer <- function(input = NA, transfers = NULL, ...) {
    if (is.null(transfers))
        transfers <- list()
    x <- fe(children = transfers, ...)
    if (! is.na(input))
        x$`in` <- input
    class(x) <- c("fe.component.transfer", class(x))
    x
}

addComponentFunction <- function(ct, channel = c("R", "G", "B", "A"), func) {
    if (! inherits(ct, "fe.component.transfer"))
        stop("'ct' must be a 'fe.component.transfer' object")
    if (! inherits(func, "transfer.function"))
        stop("'func' must be a 'transfer.function' object")
    ct$children[[channel]] <- func
    ct
}

filterSVG.transfer.function <- function(x, dev) {
    svgdev <- dev@dev

    # Need to format tableValues as a whitespace/comma separated list
    if (x$type == "table" | x$type == "discrete")
        x$tableValues <- paste0(round(x$tableValues, 2), collapse = " ")

    newXMLNode(paste0("feFunc", x$channel),
               attrs = roundAttribs(x), parent = svgDevParent(svgdev))
}

transferFunction <- function(type = c("identity", "table", "discrete", "linear", "gamma"),
                             tableValues = numeric(),
                             slope = 1, intercept = 0,
                             amplitude = 1, exponent = 1, offset = 0) {
    x <- list(type = match.arg(type))
    if (x$type == "table" | x$type == "discrete") {
        if (! length(tableValues))
            stop("A non-zero vector of numeric values must be provided")
        x$tableValues <- tableValues
    }
    if (x$type == "linear") {
        x$slope <- slope
        x$intercept <- intercept
    }
    if (x$type == "") {
        x$amplitude <- amplitude
        x$exponent <- exponent
        x$offset <- offset
    }
    class(x) <- "transfer.function"
    x
}

filterSVG.fe.composite <- function(x, dev) {
    svgdev <- dev@dev
    newXMLNode("feComposite",
               attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feComposite <- function(input1 = NA, input2 = NA,
                        operator = c("over", "in", "out", "atop", "xor", "arithmetic"),
                        k1 = 0, k2 = 0, k3 = 0, k4 = 0, ...) {
    x <- fe(operator = match.arg(operator), ...)
    if (! is.na(input1))
        x$`in` <- input1
    if (! is.na(input2))
        x$in2 <- input2
    if (x$operator == "arithmetic") {
        x$k1 <- k1
        x$k2 <- k2 
        x$k3 <- k3
        x$k4 <- k4
    }
    class(x) <- c("fe.composite", class(x))
    x
}

filterSVG.fe.convolve.matrix <- function(x, dev) {
    svgdev <- dev@dev

    if (length(x$order) > 1)
        x$order <- paste0(x$order, collapse = " ")

    if (! is.null(x$kernelUnitLength))
        x$kernelUnitLength <- paste0(round(x$kernelUnitLength, 2),
                                     collapse = " ")

    x$kernelMatrix <- paste0(apply(x$kernelMatrix, 1, function(x) {
        paste0(round(x, 2), collapse = " ")
    }), collapse = " ")

    newXMLNode("feConvolveMatrix",
               attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feConvolveMatrix <- function(input = NA, order = 3,
                             kernelMatrix = matrix(),
                             divisor = 1, bias = 0,
                             targetX = 1, targetY = 1,
                             edgeMode = c("duplicate", "wrap", "none"),
                             kernelUnitLength = NA,
                             preserveAlpha = FALSE, ...) {
    # Note that defaults for targetX and targetY are: floor(order[1:2] / 2)
    # This is going to be 1 by default, as floor(1.5) is 1
    if (length(order) == 1)
        order <- rep(order, 2)

    if (length(kernelMatrix) != (order[1] * order[2]))
        stop("Invalid number of entries for 'kernelMatrix'")

    x <- fe(order = order, kernelMatrix = kernelMatrix,
            divisor = divisor, bias = bias, targetX = targetX,
            targetY = targetY, edgeMode = match.arg(edgeMode),
            preserveAlpha = preserveAlpha, ...)
    if (! is.na(input))
        x$`in` <- input
    if (! is.na(kernelUnitLength)) {
        if (length(kernelUnitLength) == 1)
            kernelUnitLength <- rep(kernelUnitLength, 2)
        x$kernelUnitLength <- kernelUnitLength
    }
    class(x) <- c("fe.convolve.matrix", class(x))
    x
}

flatten.fe.diffuse.lighting <- function(x, coords = TRUE) {
    x$lightSource <- flatten(x$lightSource, coords)
    NextMethod()
}

filterSVG.fe.diffuse.lighting <- function(x, dev) {
    svgdev <- dev@dev

    if (! is.null(x$kernelUnitLength))
        x$kernelUnitLength <- paste0(round(x$kernelUnitLength, 2),
                                     collapse = " ")

    fedl <- newXMLNode("feDiffuseLighting",
                       attrs = roundAttribs(x),
                       parent = svgDevParent(svgdev))
    svgDevChangeParent(fedl, svgdev)
    filterSVG(x$lightSource, dev)
    svgDevChangeParent(xmlParent(fedl), svgdev)
}

feDiffuseLighting <- function(input = NA, surfaceScale = 1,
                              diffuseConstant = 1, kernelUnitLength = NA,
                              col = "white", lightSource = NULL, ...) {
    if (is.null(lightSource))
        stop("A light source must be provided")
    if (diffuseConstant < 0)
        stop("'diffuseConstant' must be non-negative")
    x <- fe(surfaceScale = surfaceScale, diffuseConstant = diffuseConstant,
            "lighting-color" = c(col2rgb(col)), lightSource = lightSource,
            ...)
    if (! is.na(input))
        x$`in` <- input
    if (! is.na(kernelUnitLength)) {
        if (length(kernelUnitLength) == 1)
            kernelUnitLength <- rep(kernelUnitLength, 2)
        x$kernelUnitLength <- kernelUnitLength
    }
    class(x) <- c("fe.diffuse.lighting", class(x))
    x
}

filterSVG.fe.displacement.map <- function(x, dev) {
    svgdev <- dev@dev
    newXMLNode("feDisplacementMap",
               attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feDisplacementMap <- function(input1 = NA, input2 = NA, scale = 0,
                              xChannelSelector = c("A", "R", "G", "B"),
                              yChannelSelector = c("A", "R", "G", "B"),
                              ...) {
    x <- fe(scale = scale,
            xChannelSelector = match.arg(xChannelSelector),
            yChannelSelector = match.arg(yChannelSelector), ...)
    if (! is.na(input1))
        x$`in` <- input1
    if (! is.na(input2))
        x$in2 <- input2
    class(x) <- c("fe.displacement.map", class(x))
    x
}

filterSVG.fe.flood <- function(x, dev) {
    svgdev <- dev@dev
    newXMLNode("feFlood",
               attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feFlood <- function(col = "black", ...) {
    cols <- c(col2rgb(col, alpha = TRUE))
    x <- fe("flood-color" = paste0("rgb(", paste0(cols[1:3], collapse = ", "), ")"),
            "flood-opacity" = cols[4] / 255, ...)
    class(x) <- c("fe.flood", class(x))
    x
}

filterSVG.fe.gaussian.blur <- function(x, dev) {
    svgdev <- dev@dev
    if (length(x$stdDeviation) > 1)
        x$sd <- paste0(round(x$stdDeviation, 2), collapse = " ")
    newXMLNode("feGaussianBlur",
               attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feGaussianBlur <- function(input = NA, sd = 0, ...) {
    x <- fe(stdDeviation = sd, ...)
    if (! is.na(input))
        x$`in` <- input
    class(x) <- c("fe.gaussian.blur", class(x))
    x
}

filterSVG.fe.image <- function(x, dev) {
    svgdev <- dev@dev
    newXMLNode("feImage",
               attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feImage <- function(preserveAspectRatio = "xMidYMid meet", href = "", ...) {
    # Docs: http://www.w3.org/TR/SVG/coords.html#PreserveAspectRatioAttribute
    x <- fe(preserveAspectRatio = preserveAspectRatio,
            externalResourcesRequired = TRUE,
            "xlink:href" = href, ...)
    class(x) <- c("fe.image", class(x))
    x
}

filterSVG.fe.merge <- function(x, dev) {
    svgdev <- dev@dev
    children <- x$mergeNodes
    par <- cleanAttrs(x, "mergeNodes")
    merge <- newXMLNode("feMerge",
                        attrs = roundAttribs(par),
                        parent = svgDevParent(svgdev))
    if (! length(children))
        return()
    svgDevChangeParent(merge, svgdev)
    for (i in 1:length(children))
        filterSVG(children[[i]], dev)
    svgDevChangeParent(xmlParent(merge), svgdev)
}

feMerge <- function(mergeNodes = NULL, ...) {
    if (is.null(mergeNodes))
        mergeNodes <- list()
    if (inherits(mergeNodes, "fe.merge.node"))
        mergeNodes <- list(mergeNodes)
    x <- fe(mergeNodes = mergeNodes, ...)
    class(x) <- c("fe.merge", class(x))
    x
}

filterSVG.fe.merge.node <- function(x, dev) {
    svgdev <- dev@dev
    newXMLNode("feMergeNode", attrs = x, parent = svgDevParent(svgdev))
}

feMergeNode <- function(input = NA) {
    x <- if (! is.na(input)) list("in" = input)
         else list()
    class(x) <- "fe.merge.node"
    x
}

addMergeNode <- function(fe, mergeNode, after = NA) {
    if (! inherits(fe, "fe.merge"))
        stop("'fe' must be a 'fe.merge' object")
    if (! inherits(mergeNode, "fe.merge.node"))
        stop("'mergeNode' must be a 'fe.merge.node' object")
    if (is.na(after))
        after <- length(fe$children)
    fe$children[[after + 1]] <- mergeNode
    fe
}

filterSVG.fe.morphology <- function(x, dev) {
    svgdev <- dev@dev
    x$radius <- paste0(round(x$radius, 2), collapse = " ")
    newXMLNode("feMorphology", attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

flatten.fe.morphology <- function(x, coords = TRUE) {
    if (coords) {
        if (length(x$radius) > 1) {
            rx <- convertX(x$radius[1], "inches")
            ry <- convertY(x$radius[2], "inches")
            x$radius <- unit.c(rx, ry)
        } else {
            x$radius <- dToInches(x$radius, NULL)
        }
    } else {
        if (length(x$radius) > 1) {
            rx <- convertX(x$radius[1], "npc", valueOnly = TRUE)
            ry <- convertY(x$radius[2], "npc", valueOnly = TRUE)
            x$radius <- unit.c(rx, ry)
        } else {
            # Just use X for radius
            x$radius <- convertX(dToInches(x$radius, NULL), "npc",
                                 valueOnly = TRUE)
        }
    }
    NextMethod()
}

feMorphology <- function(input = NA,
                         operator = c("erode", "dilate"),
                         radius = unit(0, "npc"),
                         default.units = "npc", ...) {
    if (! is.unit(radius))
        radius <- unit(radius, default.units)
    x <- fe(operator = match.arg(operator), radius = radius, ...)
    if (! is.na(input))
        x$`in` <- input
    class(x) <- c("fe.morphology", class(x))
    x
}

flatten.fe.offset <- function(x, coords = TRUE) {
    if (coords) {
        x$dx <- convertWidth(x$dx, "inches")
        x$dy <- convertHeight(x$dy, "inches")
    } else {
        x$dx <- convertWidth(x$dx, "npc", valueOnly = TRUE)
        x$dy <- convertHeight(x$dy, "npc", valueOnly = TRUE)
    }

    NextMethod()
}

filterSVG.fe.offset <- function(x, dev) {
    svgdev <- dev@dev
    x$dx <- cx(x$dx, dev)
    x$dy <- cy(x$dy, dev)
    newXMLNode("feOffset", attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feOffset <- function(input = NA,
                     dx = unit(0, "npc"), dy = unit(0, "npc"),
                     default.units = "npc", ...) {
    if (! is.unit(dx))
        dx <- unit(dx, default.units)
    if (! is.unit(dy))
        dy <- unit(dy, default.units)
    x <- fe(dx = dx, dy = dy, ...)
    if (! is.na(input))
        x$`in` <- input
    class(x) <- c("fe.offset", class(x))
    x
}

flatten.fe.specular.lighting <- function(x, coords = TRUE) {
    x$lightSource <- flatten(x$lightSource, coords)
    NextMethod()
}

filterSVG.fe.specular.lighting <- function(x, dev) {
    svgdev <- dev@dev
    if (! is.null(x$kernelUnitLength))
        x$kernelUnitLength <- paste0(round(x$kernelUnitLength, 2),
                                     collapse = " ")
    fesl <- newXMLNode("feSpecularLighting",
                       attrs = roundAttribs(x),
                       parent = svgDevParent(svgdev))
    svgDevChangeParent(fesl, svgdev)
    filterSVG(x$lightSource, dev)
    svgDevChangeParent(xmlParent(fesl), svgdev)
}

feSpecularLighting <- function(input = NA, surfaceScale = 1,
                               specularConstant = 1, specularExponent = 1,
                               kernelUnitLength = NA, lightSource = NULL, ...) {
    if (is.null(lightSource))
        stop("A light source must be provided")
    if (specularConstant < 0)
        stop("'specularConstant' must be non-negative")
    if (specularExponent < 1) {
        warning("exponent less than 1, increasing to 1")
        specularExponent <- 1
    } else if (specularExponent > 128) {
        warning("exponent larger than 128, reducing to 128")
        specularExponent <- 128
    }
    x <- fe(surfaceScale = surfaceScale,
            specularConstant = specularConstant, specularExponent = specularExponent,
            "lighting-color" = c(col2rgb(col)), lightSource = lightSource, ...)
    if (! is.na(input))
        x$`in` <- input
    if (! is.na(kernelUnitLength)) {
        if (length(kernelUnitLength) == 1)
            kernelUnitLength <- rep(kernelUnitLength, 2)
        x$kernelUnitLength <- kernelUnitLength
    }
    class(x) <- c("fe.specular.lighting", class(x))
    x
}

filterSVG.fe.tile <- function(x, dev) {
    svgdev <- dev@dev
    newXMLNode("feTile", attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feTile <- function(input = NA, ...) {
    x <- fe(...)
    if (! is.na(input))
        x$`in` <- input
    class(x) <- c("fe.tile", class(x))
    x
}

filterSVG.fe.turbulence <- function(x, dev) {
    svgdev <- dev@dev
    if (length(x$baseFrequency) > 1)
        x$baseFrequency <- paste0(round(x$baseFrequency, 2),
                                  collapse = " ")
    newXMLNode("feTurbulence", attrs = roundAttribs(x),
               parent = svgDevParent(svgdev))
}

feTurbulence <- function(baseFrequency = 0, numOctaves = 1, seed = 1,
                         stitchTiles = FALSE,
                         type = c("turbulence", "fractalNoise"), ...) {
    stitchTiles <- if (stitchTiles) "stitch" else "noStitch"
    x <- fe(baseFrequency = baseFrequency, numOctaves = numOctaves, seed = seed,
            stitchTiles = stitchTiles, type = match.arg(type), ...)
    class(x) <- c("fe.turbulence", class(x))
    x
}

