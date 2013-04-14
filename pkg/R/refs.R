# This file is concerned with the use of objects that need to be
# referenced, e.g. pattern fills, gradient fills, filters, etc.

# We set up the definitions here but they actually need to be created at
# *draw time* because we need to be able to calculate SVG coordinates
# using the SVG device (particularly for the x, y, width, height info).
# Because the location is being defined at the time of definition, the
# current viewport is determining the location of the pattern, not the
# graphics device.

pattern <- function(grob = NULL,
                    x = unit(0, "npc"), y = unit(0, "npc"),
                    width = unit(0.1, "npc"), height = unit(0.1, "npc"),
                    default.units = "npc",
                    just = "centre", hjust = NULL, vjust = NULL,
                    dev.width = 7, dev.height = 7) {
    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(width))
        width <- unit(width, default.units)
    if (! is.unit(height))
        height <- unit(height, default.units)

    pattern <- list(grob = grob,
                    x = x, y = y,
                    width = width, height = height,
                    just = just, hjust = hjust, vjust = vjust,
                    dev.width = dev.width, dev.height = dev.height)
    class(pattern) <- "pattern"
    pattern
}

registerPatternFill <- function(label, pattern = NULL, ...) {
    checkExistingDefinition(label)
    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)

    if (is.null(pattern)) {
        pattern <- gridSVG::pattern(...)
    } else if (! inherits(pattern, "pattern")) {
        stop("'pattern' must be a 'pattern' object")
    }

    if (is.null(pattern$grob))
        stop("A grob must be given for a fill pattern definition")

    # Now convert *at time of definition* to absolute units (inches)
    loc <- leftbottom(pattern$x, pattern$y, pattern$width, pattern$height,
                      pattern$just, pattern$hjust, pattern$vjust, NULL)
    x <- loc$x
    y <- loc$y
    width <- convertWidth(pattern$width, "inches")
    height <- convertHeight(pattern$height, "inches")

    # ID will be overwritten later, because we might change
    # the separator used for "id.sep"
    defList <- list(
        label = label,
        id = getID(label, "ref"),
        grob = pattern$grob,
        vp = pattern$vp,
        x = x,
        y = y,
        width = width,
        height = height,
        dev.width = pattern$dev.width,
        dev.height = pattern$dev.height
    )

    class(defList) <- "patternFillDef"

    refDefinitions[[label]] <- defList
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

registerPatternFillRef <- function(label, refLabel, pattern = NULL, ...) {
    checkExistingDefinition(label)
    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)
    if (! refLabel %in% names(refDefinitions))
        stop(paste("The reference labelled", sQuote(label), "does not exist."))

    if (is.null(pattern)) {
        pattern <- gridSVG::pattern(...)
    } else if (! inherits(pattern, "pattern")) {
        stop("'pattern' must be a 'pattern' object")
    }

    # Now convert *at time of definition* to absolute units (inches)
    loc <- leftbottom(pattern$x, pattern$y,
                      pattern$width, pattern$height,
                      pattern$just, pattern$hjust, pattern$vjust, NULL)
    x <- loc$x
    y <- loc$y
    width <- convertWidth(pattern$width, "inches")
    height <- convertHeight(pattern$height, "inches")

    defList <- list(
        label = label,
        refLabel = refLabel,
        id = getID(label, "ref"),
        x = x,
        y = y,
        width = width,
        height = height
    )

    class(defList) <- "patternFillRefDef"

    refDefinitions[[label]] <- defList
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

drawDef <- function(def, dev) {
    UseMethod("drawDef")
}

drawDef.patternFillDef <- function(def, dev) {
    svgdev <- dev@dev

    # Convert grid coords to SVG coords
    x <- round(cx(def$x, dev), 2)
    y <- round(cy(def$y, dev), 2)
    width <- round(cw(def$width, dev), 2)
    height <- round(ch(def$height, dev), 2)

    # Attempt to use a known-safe prefix
    # If the prefix is safe, then it will *always* be safe
    # because the names are known *after* content is drawn
    # and the referenced labels must be unique
    prefix <- paste0("gridSVG.patternFill.", def$id)

    # There is a little bit of replication going on from
    # 'gridToSVG' but it avoids some problems.
    # We could use 'gridToSVG' recursively but we lose the ability to
    # definitely generate unique names if that is the case because usage
    # tables would be wiped.
    # A viewport and gTree are forced to ensure everything is unique because
    # we want paths to be used.
    # We do not care at this point whether it is strictly necessary to
    # perform all of this because we just want unique IDs.
    pdf(file = NULL, width = def$dev.width, height = def$dev.height)
        newdev <- openSVGDev("", res = dev@res,
                             width = par("din")[1], height = par("din")[2])
        pushViewport(viewport(name = getID(prefix, "vp")))
        if (! is.null(def$vp))
            pushViewport(def$vp)
        grid.draw(gTree(name = getID(prefix, "grob"),
                  children = gList(def$grob),
                  gp = get.gpar())) # Force gp to ensure correct styling
        grid.force(redraw = FALSE)
        gt <- grid.grab(name = "gridSVG", wrap = TRUE)
        gridToDev(gt, newdev)
        newroot <- devClose(newdev)
        viewBox <- xmlGetAttr(newroot, "viewBox")
        gridSVGNode <- prefixName("gridSVG")
        gridSVGNode <- getNodeSet(newroot, paste0("//*[@id='", gridSVGNode, "']"))[[1]]
    dev.off()

    # Creating the pattern element
    pattern <- newXMLNode("pattern",
                          attrs = list(id = prefixName(def$id), x = x, y = y,
                                       width = width, height = height,
                                       viewBox = viewBox,
                                       patternUnits = "userSpaceOnUse"),
                          parent = svgDevParent(svgdev))
    # Assigning its children
    xmlChildren(pattern) <- xmlChildren(gridSVGNode)
}

drawDef.patternFillRefDef <- function(def, dev) {
    svgdev <- dev@dev

    # Convert grid coords to SVG coords
    x <- round(cx(def$x, dev), 2)
    y <- round(cy(def$y, dev), 2)
    width <- round(cw(def$width, dev), 2)
    height <- round(ch(def$height, dev), 2)

    # Creating the pattern element
    pattern <- newXMLNode("pattern",
        attrs = list(id = prefixName(def$id), x = x, y = y,
                     width = width, height = height,
                     "xlink:href" =
                       paste0("#", getLabelID(def$refLabel))),
        parent = svgDevParent(svgdev))
}

# This function ensures that if we change the ID separator value between
# the time of definition and draw time, we still get the expected IDs.
assignRefIDs <- function() {
    refdefs <- get("refDefinitions", envir = .gridSVGEnv)
    for (i in seq_along(refdefs))
        refdefs[[i]]$id <- getLabelID(refdefs[[i]]$label)
    assign("refDefinitions", refdefs, envir = .gridSVGEnv)

    # Because the separators might have changed, ensure that the
    # usageTable has the correct (escaped) values for selectors and xpath
    ut <- get("usageTable", envir = .gridSVGEnv)
    inds <- which(ut$type == "ref")
    for (i in inds) {
        fullName <- paste(ut[i, "name"],
                          ut[i, "suffix"],
                          sep = getSVGoption("id.sep"))
        sel <- prefixName(escapeSelector(fullName))
        xp <- prefixName(escapeXPath(fullName))
        ut[i, "selector"] <- sel
        ut[i, "xpath"] <- xp
    }
    assign("usageTable", ut, envir = .gridSVGEnv)
}

flushDefinitions <- function(dev) {
    svgdev <- dev@dev

    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)
    if (! length(refDefinitions))
        return()

    # Keep copies of old tables because they will be modified when
    # we draw any children
    usageTable <- get("usageTable", envir = .gridSVGEnv)
    vpCoords <- get("vpCoords", envir = .gridSVGEnv)
    use.vpPaths <- get("use.vpPaths", envir = .gridSVGEnv)
    use.gPaths <- get("use.gPaths", envir = .gridSVGEnv)
    uniqueNames <- get("uniqueNames", envir = .gridSVGEnv)

    # Set required options -- we don't care about what the user
    # has specified at this point because they shouldn't be
    # touching any reference definitions
    assign("use.vpPaths", TRUE, envir = .gridSVGEnv)    
    assign("use.gPaths", TRUE, envir = .gridSVGEnv)
    assign("uniqueNames", TRUE, envir = .gridSVGEnv)
    
    # Begin creating definitions
    # First ensure we're under #gridSVG
    rootID <- prefixName("gridSVG")
    gridSVGNode <- getNodeSet(svgDevParent(svgdev),
                              paste0("//*[@id='", rootID, "']"))[[1]]
    svgDevChangeParent(gridSVGNode, svgdev)
    defs <- newXMLNode("defs", parent = svgDevParent(svgdev))
    svgDevChangeParent(defs, svgdev)

    # Check whether we have any dependent references, e.g. have a pattern
    # fill by reference in use but not the pattern itself. We need to ensure
    # that both are written out.
    rut <- get("refUsageTable", envir = .gridSVGEnv)
    for (i in 1:length(refDefinitions)) {
        def <- refDefinitions[[i]]
        if (isLabelUsed(def$label) &&
            ! is.null(def$refLabel) && ! isLabelUsed(def$refLabel))
            rut[rut$label == def$refLabel, "used"] <- TRUE
    }    
    assign("refUsageTable", rut, envir = .gridSVGEnv) 

    # Now try drawing
    for (i in 1:length(refDefinitions)) {
        def <- refDefinitions[[i]]
        if (isLabelUsed(def$label))
            drawDef(def, dev)
    }

    # Resetting to original values
    assign("vpCoords", vpCoords, envir = .gridSVGEnv)
    assign("use.vpPaths", use.vpPaths, envir = .gridSVGEnv)
    assign("use.gPaths", use.gPaths, envir = .gridSVGEnv)
    assign("uniqueNames", uniqueNames, envir = .gridSVGEnv)

    # Reset ref usage table
    rut <- get("refUsageTable", envir = .gridSVGEnv)
    rut$used <- logical(nrow(rut))
    assign("refUsageTable", rut, envir = .gridSVGEnv)
    # Again for usage table 
    assign("usageTable", usageTable, envir = .gridSVGEnv)

    # Get out of defs
    svgDevChangeParent(xmlParent(defs), svgdev)
}

anyRefsDefined <- function() {
    ut <- get("usageTable", envir = .gridSVGEnv)
    nrow(ut) > 0 && any(ut$type == "ref")
}

# Used for knowing whether to write out a definition.
# If a definition has not been used we do not write it out.
# If it has been used more than once we do not repeat the
# definition.
isLabelUsed <- function(label) {
    rut <- get("refUsageTable", envir = .gridSVGEnv)
    rut[rut$label == label, "used"]
}

grid.patternFill <- function(path, pattern = NULL, label = NULL,
                             alpha = 1, group = TRUE, grep = FALSE) {
    if (is.null(label) & is.null(pattern)) {
        stop("At least one of 'label' or 'pattern' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.patternFill")
        registerPatternFill(label, pattern)
    } else if (is.null(pattern)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerPatternFill(label, pattern)
        pattern <- NULL # use the ref from now on
    }

    if (any(grep)) {
        grobApply(path, function(path) {
            grid.set(path, patternFillGrob(grid.get(path), label = label,
                                           pattern = pattern, alpha = alpha,
                                           group = group))
        }, grep = grep)
    } else {
        grid.set(path,
                 patternFillGrob(grid.get(path), pattern = pattern, label = label,
                                 alpha = alpha, group = group))
    }
}

patternFillGrob <- function(x, pattern = NULL, label = NULL,
                            alpha = 1, group = TRUE) {
    if (is.null(label) & is.null(pattern)) {
        stop("At least one of 'label' or 'pattern' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.patternFill")
        registerPatternFill(label, pattern)
    } else if (is.null(pattern)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerPatternFill(label, pattern)
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
    class(x) <- unique(c("patternFilled.grob", "referring.grob", class(x)))
    x
}

primToDev.referring.grob <- function(x, dev) {
    # Now that we know a reference is being used, ensure that it can be
    # written out later
    rut <- get("refUsageTable", envir = .gridSVGEnv)
    rut[rut$label == x$label, "used"] <- TRUE
    assign("refUsageTable", rut, envir = .gridSVGEnv)
    NextMethod()
}

checkForDefinition <- function(label) {
    if (! label %in% names(get("refDefinitions", envir = .gridSVGEnv)))
        stop("A reference definition must be created before using this label")
}

checkExistingDefinition <- function(label) {
    if (label %in% names(get("refDefinitions", envir = .gridSVGEnv)))
        stop(paste("label", sQuote(label),
                   "already exists as a reference definition")) 
}

# When we need to generate a temporary label (i.e. when specifying a
# gradient fill directly on a grob with no label), we supply a prefix and
# return a new label that is going to be unique (among labels).
# getID() will perform the task of ensuring uniqueness among IDs.
getNewLabel <- function(prefix) {
    i <- 1
    candidateName <- paste0(prefix, ".", i)
    refdefs <- get("refDefinitions", envir = .gridSVGEnv)
    while(candidateName %in% names(refdefs)) {
        i <- i + 1
        candidateName <- paste0(prefix, getSVGoption("id.sep"), i)
    }
    candidateName
}

getLabelID <- function(label) {
    ut <- get("usageTable", envir = .gridSVGEnv)
    suffix <- ut[ut$name == label & ut$type == "ref", "suffix"]
    prefixName(paste0(label, getSVGoption("id.sep"), suffix))
}

