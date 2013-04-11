# High level functions for applying clipping paths to grobs.
grid.clipPath <- function(path, label = NULL,
                          clippath = NULL, cliprule = "winding",
                          group = TRUE, grep = FALSE) {
    if (is.null(label) & is.null(clippath)) {
        stop("At least one of 'label' or 'clippath' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.clipPath")
        clipPath(label, clippath)
    } else if (is.null(clippath)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        clipPath(label, clippath)
        clipPath <- NULL # use the ref from now on
    }

    if (any(grep)) {
        grobApply(path, function(path) {
            grid.set(path, clipPathGrob(grid.get(path), label = label,
                                        clippath = clippath, cliprule = cliprule,
                                        group = group))
        }, grep = grep)
    } else {
        grid.set(path,
                 clipPathGrob(grid.get(path), label = label,
                              clippath = clippath, cliprule = cliprule,
                              group = group))
    }
}

clipPathGrob <- function(x, label = NULL,
                         clippath = NULL, cliprule = "winding",
                         group = TRUE) {
    if (is.null(label) & is.null(clippath)) {
        stop("At least one of 'label' or 'clippath' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.clippath")
        clipPath(label, clippath)
    } else if (is.null(clippath)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        clipPath(label, clippath)
    }

    # grid.path rule -> SVG clip-rule
    if (cliprule == "winding")
        cliprule <- "nonzero"

    x$label <- label
    label <- getLabelID(label)
    x <- garnishGrob(x,
                     "clip-path" = paste0("url(#", label, ")"),
                     "clip-rule" = cliprule,
                     group = group)
    class(x) <- unique(c("pathClipped.grob", "referring.grob", class(x)))
    x
}

# Assign a label to a clip path and fix it at this point
clipPath <- function(label, clippath = NULL, ...) {
    checkExistingDefinition(label)
    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)

    if (is.null(clippath))
        clippath <- createClippingPath(...)
    if (! inherits(clippath, "clipping.path"))
        stop("'clippath' must be a 'clipping.path' object")
    if (is.null(clippath$grob))
        stop("A grob must be given for a clipping path definition")

    # ID will be overwritten later, because we might change
    # the separator used for "id.sep"
    defList <- list(
        label = label,
        id = getID(label, "ref"),
        grob = clippath$grob,
        vp = clippath$vp
    )

    class(defList) <- "clippathDef"

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

createClippingPath <- function(grob = NULL, vp = NULL) {
    clippath <- list(grob = grob, vp = vp)
    class(clippath) <- "clipping.path"
    clippath
}

drawDef.clippathDef <- function(def, dev) {
    svgdev <- dev@dev

    # Attempt to use a known-safe prefix
    # If the prefix is safe, then it will *always* be safe
    # because the names are known *after* content is drawn
    # and the referenced labels must be unique
    prefix <- paste0("gridSVG.clipPath.", def$id)

    # There is a little bit of replication going on from
    # 'gridToSVG' but it avoids some problems.
    # We could use 'gridToSVG' recursively but we lose the ability to
    # definitely generate unique names if that is the case because usage
    # tables would be wiped.
    # A viewport and gTree are forced to ensure everything is unique because
    # we want paths to be used.
    # We do not care at this point whether it is strictly necessary to
    # perform all of this because we just want unique IDs.
    pdf(file = NULL, width = dev@width, height = dev@width)
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
        gridSVGNode <- prefixName("gridSVG")
        gridSVGNode <- getNodeSet(newroot, paste0("//*[@id='", gridSVGNode, "']"))[[1]]
    dev.off()

    nodelist <- flattenClippedSVG(gridSVGNode)

    # Creating the clipPath element
    clippath <- newXMLNode("clipPath",
                           attrs = list(id = prefixName(def$id)),
                           parent = svgDevParent(svgdev))
    # Assigning its children
    xmlChildren(clippath) <- nodelist
}

flattenClippedSVG <- function(node) {
    # Mostly taken from spec, only adding in what we use though
    # Omitted - animation elements, 'use', 'ellipse', 'line'
    validElements <- c("animate", "animateTransform", "circle", "path",
                       "polygon", "polyline", "rect", "text")
    subset <- getNodeSet(node,
                         paste0("//svg:", validElements, collapse = " | "),
                         c(svg = "http://www.w3.org/2000/svg"))
    for (i in 1:length(subset)) {
        el <- subset[[i]]
        name <- xmlName(el)
        if (name == "text") {
            # We know that the structure is:
            # <g ....>
            #   <g scale>
            #     <text ...>
            p <- xmlParent(el)
            gp <- xmlParent(p)
            gpattrs <- xmlAttrs(gp)
            gpattrs["transform"] <- paste(gpattrs["transform"],
                                          xmlAttrs(p)["transform"])
            # There might also be a rotation present on the text itself
            if ("transform" %in% names(xmlAttrs(el)))
                gpattrs["transform"] <- paste(gpattrs["transform"],
                                              xmlAttrs(el)["transform"])
            xmlAttrs(el) <- gpattrs
        }
    }
    subset
}

# High level functions for applying masks to grobs,
# but not viewports because they already have their own clipping settings.
# If we messed with those then we would have to overwrite existing definitions
# for clipping paths on viewports. Not consistent.
grid.mask <- function(path, label = NULL, mask = NULL,
                      group = TRUE, grep = FALSE) {
    if (is.null(label) & is.null(mask)) {
        stop("At least one of 'label' or 'mask' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.mask")
        defineMask(label, mask)
        mask <- NULL
    } else if (is.null(mask)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        defineMask(label, mask)
        mask <- NULL # use the ref from now on
    }

    if (any(grep)) {
        grobApply(path, function(path) {
            grid.set(path, maskGrob(grid.get(path), label = label,
                                    mask = mask, group = group))
        }, grep = grep)
    } else {
        grid.set(path,
                 maskGrob(grid.get(path), label = label,
                          mask = mask, group = group))
    }
}

maskGrob <- function(x, label = NULL, mask = NULL, group = TRUE) {
    if (is.null(label) & is.null(mask)) {
        stop("At least one of 'label' or 'mask' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.mask")
        defineMask(label, mask)
    } else if (is.null(mask)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        defineMask(label, mask)
    }

    x$label <- label
    label <- getLabelID(label)
    x <- garnishGrob(x, mask = paste0("url(#", label, ")"), group = group)
    class(x) <- unique(c("masked.grob", "referring.grob", class(x)))
    x
}

defineMask <- function(label, mask = NULL, ...) {
    checkExistingDefinition(label)
    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)

    if (is.null(mask))
        mask <- createMask(...)
    if (! inherits(mask, "mask"))
        stop("'mask' must be a 'mask' object")
    if (is.null(mask$grob))
        stop("A grob must be given for a mask definition")

    # Now convert *at time of definition* to absolute units (inches)
    loc <- leftbottom(mask$x, mask$y, mask$width, mask$height,
                      mask$just, mask$hjust, mask$vjust, NULL)
    mask$x <- loc$x
    mask$y <- loc$y
    mask$width <- convertWidth(mask$width, "inches")
    mask$height <- convertHeight(mask$height, "inches")

    # ID will be overwritten later, because we might change
    # the separator used for "id.sep"
    defList <- list(
        label = label,
        id = getID(label, "ref"),
        grob = mask$grob,
        vp = mask$vp,
        x = mask$x,
        y = mask$y,
        width = mask$width,
        height = mask$height
    )

    class(defList) <- "maskDef"

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

createMask <- function(grob = NULL, vp = NULL,
                       x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       width = unit(1, "npc"), height = unit(1, "npc"),
                       default.units = "npc",
                       just = "centre", hjust = NULL, vjust = NULL) {
    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(width))
        width <- unit(width, default.units)
    if (! is.unit(height))
        height <- unit(height, default.units)

    mask <- list(grob = grob, vp = vp,
                 x = x, y = y,
                 width = width, height = height,
                 just = just, hjust = hjust, vjust = vjust)
    class(mask) <- "mask"
    mask
}

drawDef.maskDef <- function(def, dev) {
    svgdev <- dev@dev

    # Convert grid coords to SVG coords
    def$x <- round(cx(def$x, dev), 2)
    def$y <- round(cy(def$y, dev), 2)
    def$width <- round(cw(def$width, dev), 2)
    def$height <- round(ch(def$height, dev), 2)

    # Attempt to use a known-safe prefix
    # If the prefix is safe, then it will *always* be safe
    # because the names are known *after* content is drawn
    # and the referenced labels must be unique
    prefix <- paste0("gridSVG.mask.", def$id)

    # There is a little bit of replication going on from
    # 'gridToSVG' but it avoids some problems.
    # We could use 'gridToSVG' recursively but we lose the ability to
    # definitely generate unique names if that is the case because usage
    # tables would be wiped.
    # A viewport and gTree are forced to ensure everything is unique because
    # we want paths to be used.
    # We do not care at this point whether it is strictly necessary to
    # perform all of this because we just want unique IDs.
    #
    # Also try and replicate the exact same graphics device, but with a
    # new clean page.
    pdf(file = NULL, width = dev@width, height = dev@height)
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
        gridSVGNode <- prefixName("gridSVG")
        gridSVGNode <- getNodeSet(newroot, paste0("//*[@id='", gridSVGNode, "']"))[[1]]
    dev.off()

    # Creating the mask element
    mask <- newXMLNode("mask",
                       attrs = list(id = prefixName(def$id),
                                    x = def$x, y = def$y,
                                    width = def$width, height = def$height,
                                    maskUnits = "userSpaceOnUse"),
                       parent = svgDevParent(svgdev))
    # Assigning its children
    xmlChildren(mask) <- xmlChildren(gridSVGNode)
}


