# This file is concerned with the use of objects that need to be
# referenced, e.g. pattern fills, gradient fills, filters, etc.

# We set up the definitions here but they actually need to be created at
# *draw time* because we need to be able to calculate SVG coordinates
# using the SVG device (particularly for the x, y, width, height info).
# Because the location is being defined at the time of definition, the
# current viewport is determining the location of the pattern, not the
# graphics device.
fillPattern <- function(label, grob, vp = NULL,
                        x = unit(0, "npc"), y = unit(0, "npc"),
                        width = unit(0.1, "npc"), height = unit(0.1, "npc"),
                        default.units = "npc",
                        just = "centre", hjust = NULL, vjust = NULL,
                        dev.width = 7, dev.height = 7) {
    # Ensure we have unique labels for a referenced pattern def
    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)
    if (label %in% names(refDefinitions))
        stop(paste("label", sQuote(label),
                   "already exists as a reference definition."))

    if (missing(grob))
        stop("A grob must be given as a fill pattern.")

    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(width))
        width <- unit(width, default.units)
    if (! is.unit(height))
        height <- unit(height, default.units)

    # Now convert *at time of definition* to absolute units (inches)
    loc <- leftbottom(x, y, width, height, just, hjust, vjust, NULL)
    x <- loc$x
    y <- loc$y
    width <- convertWidth(width, "inches")
    height <- convertHeight(height, "inches")

    defList <- list(
        label = label,
        id = getID(label, "ref"),
        grob = grob,
        vp = vp,
        x = x,
        y = y,
        width = width,
        height = height,
        dev.width = dev.width,
        dev.height = dev.height
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
    prefix <- paste0(".gridSVG.fillPattern.", def$id)

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
        gt <- grid.grab(name = "gridSVG", wrap = TRUE)
        gridToDev(gt, newdev)
        newroot <- devClose(newdev)
        viewBox <- xmlGetAttr(newroot, "viewBox")
        gridSVGNode <- getNodeSet(newroot, "//*[@id='gridSVG']")[[1]]
    dev.off()

    # Creating the pattern element
    pattern <- newXMLNode("pattern",
                          attrs = list(id = def$id, x = x, y = y,
                                       width = width, height = height,
                                       viewBox = viewBox,
                                       patternUnits = "userSpaceOnUse"),
                          parent = svgDevParent(svgdev))
    # Assigning its children
    xmlChildren(pattern) <- xmlChildren(gridSVGNode)
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
    gridSVGNode <- getNodeSet(svgDevParent(svgdev), "//*[@id='gridSVG']")[[1]]
    svgDevChangeParent(gridSVGNode, svgdev)
    defs <- newXMLNode("defs", parent = svgDevParent(svgdev))
    svgDevChangeParent(defs, svgdev)

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
    usageTable <- usageTable[usageTable$type == "ref", ]
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

grid.patternFill <- function(path, label, alpha = 1,
                             group = TRUE, grep = FALSE) {
    checkForDefinition(label)
    if (any(grep)) {
        grobApply(path, function(path) {
            grid.set(path, patternFillGrob(grid.get(path), label, alpha, group = group))
        }, grep = grep)
    } else {
        grid.set(path,
                 patternFillGrob(grid.get(path), label, alpha, group = group))
    }
}

patternFillGrob <- function(x, label, alpha = 1, group = TRUE) {
    checkForDefinition(label)
    x$label <- label
    label <- getLabelID(label)
    # Allowing fill-opacity to be set by a garnish because
    # grid only knows about a colour and its opacity. If we use a
    # reference instead of a then nothing is known about the opacity.
    # We want to ensure that we can still set it, so use the garnish
    # to overwrite it.
    x <- garnishGrob(x, fill = paste0("url(#", label, ")"),
                     "fill-opacity" = alpha, group = group)
    class(x) <- unique(c("patternFilled.grob", class(x)))
    x
}

checkForDefinition <- function(label) {
    if (! label %in% names(get("refDefinitions", envir = .gridSVGEnv)))
        stop("A reference definition must be created before using this label")
}

getLabelID <- function(label) {
    ut <- get("usageTable", envir = .gridSVGEnv)
    suffix <- ut[ut$name == label & ut$type == "ref", "suffix"]
    paste0(label, getSVGoption("id.sep"), suffix)
}

primToDev.patternFilled.grob <- function(x, dev) {
    # Now that we know a reference is being used, ensure that it can be
    # written out later
    rut <- get("refUsageTable", envir = .gridSVGEnv)
    rut[rut$label == x$label, "used"] <- TRUE
    assign("refUsageTable", rut, envir = .gridSVGEnv)
    NextMethod()
}

