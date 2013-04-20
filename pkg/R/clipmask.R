# High level functions for escaping clipping paths and masks
popContext <- function(n = 1) {
    if (n < 1)
        stop("Must pop at least one level of context")
    # Not even giving the option of configuring a name because
    # it should not be used in any serious manner
    grid.draw(grob(n = n, cl = "popContext"))
}

# We have nothing to draw here, just rip out the SVG device to
# start unwinding the tree
primToDev.popContext <- function(x, dev) {
    svgPopContext(x$n, dev@dev)
}

svgPopContext <- function(n, svgdev) {
    # IMPORTANT - clipGrobs are left alone!
    # In the case where we have reached something we know
    # is not a reference, then we don't need to unwind further.
    # This is because viewports and grobs (in particular clipGrobs)
    # will be treated separately to clipping paths and masks.
    parentIsPushContext <- function() {
        id <- xmlGetAttr(svgDevParent(svgdev), "id")
        cids <- get("contextNames", envir = .gridSVGEnv)
        id %in% cids
    }
    contextLevels <- get("contextLevels", envir = .gridSVGEnv)
    cl <- tail(contextLevels, 1)
    if (n > cl) {
        warning("An attempt was made to pop more contexts than possible, ignoring extras")
        n <- cl
    }
    # In the case where a gTree has a popContext, don't do anything because
    # it would affect any remaining children that are yet to be drawn.
    # An example:
    # pushClipPath()
    #   -> draw(gTree)
    #     -> *draw*, *draw*, *popClipPath*, *draw* <- pop will be ignored here
    #   -> leave(gTree)
    while (parentIsPushContext() && n > 0) {
        svgDevChangeParent(xmlParent(svgDevParent(svgdev)), svgdev)
        cl <- cl - 1
        n <- n - 1
    }
    contextLevels[length(contextLevels)] <- cl
    assign("contextLevels", contextLevels, envir = .gridSVGEnv)
}

###
###
###  CLIPPING PATHS
###
###

# Alias for convenient popping of a clipping path
popClipPath <- function() {
    popContext()
}

pushClipPath <- function(clippath = NULL, label = NULL,
                         name = NULL, draw = TRUE) {
    if (is.null(label) & is.null(clippath)) {
        stop("At least one of 'label' or 'clippath' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.clipPath")
        registerClipPath(label, clippath)
    } else if (is.null(clippath)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerClipPath(label, clippath)
        clippath <- NULL # use the ref from now on
    }
    cp <- grid::grob(referenceLabel = label, name = name, cl = "clipPath")
    class(cp) <- unique(c("pushClipPath", class(cp)))
    if (draw)
        grid.draw(cp)
    invisible(cp)
}

# High level functions for applying clipping paths to existing grobs
grid.clipPath <- function(path, clippath = NULL, label = NULL,
                          group = TRUE, redraw = FALSE,
                          strict = FALSE, grep = FALSE, global = FALSE) {
    if (is.null(label) & is.null(clippath)) {
        stop("At least one of 'label' or 'clippath' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.clipPath")
        registerClipPath(label, clippath)
    } else if (is.null(clippath)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerClipPath(label, clippath)
        clippath <- NULL # use the ref from now on
    }

    grobApply(path, function(path) {
        grid.set(path, clipPathGrob(grid.get(path), clippath = clippath,
                                    label = label, group = group),
                 redraw = redraw)
    }, strict = strict, grep = grep, global = global)
}

clipPathGrob <- function(x, clippath = NULL, label = NULL, group = TRUE) {
    if (is.null(label) & is.null(clippath)) {
        stop("At least one of 'label' or 'clippath' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.clipPath")
        registerClipPath(label, clippath)
    } else if (is.null(clippath)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerClipPath(label, clippath)
    }

    x$referenceLabel <- c(x$referenceLabel, label)
    label <- getLabelID(label)
    x <- garnishGrob(x, "clip-path" = paste0("url(#", prefixName(label), ")"),
                     group = group)
    class(x) <- unique(c("pathClipped.grob", class(x)))
    x
}

registerClipPath <- function(label, clippath) {
    checkExistingDefinition(label)
    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)
    
    if (! is.grob(clippath))
        stop("'clippath' must be a 'grob' object")

    defList <- list(label = label,
                    id = getID(label, "ref"),
                    grob = clippath)
    class(defList) <- "clipPathDef"
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

drawDef.clipPathDef <- function(x, dev) {
    grob <- x$grob
    # This is always going to be true because we basically assume that
    # referenced content is fixed and therefore the names don't really
    # matter.
    if (get("use.gPaths", envir = .gridSVGEnv))
        grob$name <- paste(x$label, grob$name,
                           sep = getSVGoption("gPath.sep"))
    # Start clipPath
    devStartClipPath(list(name = x$id), NULL, dev)
    # Draw grob
    grobToDev(grob, dev)
    # Close clipPath, open group
    devEndClipPath(list(name = x$id), NULL, dev)
}

primToDev.clipPath <- function(x, dev) {
    devStartClipPathGroup(devGrob(x, dev), NULL, dev)
}

devGrob.clipPath <- function(x, dev) {
    list(name = getID(x$name, "grob"), cp = getLabelID(x$label))
}

svgStartGrobClipPathGroup <- function(id = NULL, cp = NULL,
                                  svgdev = svgDevice()) {
    clipPathID <- paste0("url(#", prefixName(cp), ")")
    cp <- newXMLNode("g", attrs = list(id = prefixName(id),
                                       "clip-path" = clipPathID),
                     parent = svgDevParent(svgdev))
    svgDevChangeParent(cp, svgdev)
}

svgStartGrobClipPath <- function(id = NULL, svgdev = svgDevice()) {
    cp <- newXMLNode("clipPath", attrs = list(id = prefixName(id)),
                     parent = svgDevParent(svgdev))
    svgDevChangeParent(cp, svgdev)
}

svgEndGrobClipPath <- function(svgdev = svgDevice()) {
    # First need to collect all children and filter out unwanted content
    clippath <- svgDevParent(svgdev)
    nodelist <- flattenClippedSVG(clippath)
    # Wipe out all children, then add in the ones we want
    removeChildren(clippath, kids = xmlChildren(clippath))
    xmlChildren(clippath) <- nodelist

    # Go up one level from clipPath to defs
    svgDevChangeParent(xmlParent(svgDevParent(svgdev)), svgdev)
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

###
###
### MASKING
###
###

# Alias for popping out of a masking context
popMask <- function() {
    popContext()
}

pushMask <- function(mask = NULL, label = NULL, name = NULL, draw = TRUE) {
    if (is.null(label) & is.null(mask)) {
        stop("At least one of 'label' or 'mask' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.mask")
        registerMask(label, mask)
    } else if (is.null(mask)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerMask(label, mask)
        mask <- NULL # use the ref from now on
    }
    m <- grid::grob(referenceLabel = label, name = name, cl = "mask")
    class(m) <- unique(c("pushMask", class(m)))
    if (draw)
        grid.draw(m)
    invisible(m)
}

# High level functions for applying opacity masks to grobs
grid.mask <- function(path, mask = NULL, label = NULL,
                      group = TRUE, redraw = FALSE,
                      strict = FALSE, grep = FALSE, global = FALSE) {
    if (is.null(label) & is.null(mask)) {
        stop("At least one of 'label' or 'mask' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.mask")
        registerMask(label, mask)
    } else if (is.null(mask)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerMask(label, mask)
        mask <- NULL # use the ref from now on
    }

    grobApply(path, function(path) {
        grid.set(path, maskGrob(grid.get(path), mask = mask,
                                label = label, group = group),
                 redraw = redraw)
    }, strict = strict, grep = grep, global = global)
}

maskGrob <- function(x, mask = NULL, label = NULL, group = TRUE) {
    if (is.null(label) & is.null(mask)) {
        stop("At least one of 'label' or 'mask' must be supplied")
    } else if (is.null(label)) {
        label <- getNewLabel("gridSVG.mask")
        registerMask(label, mask)
    } else if (is.null(mask)) {
        checkForDefinition(label)
    } else {
        checkExistingDefinition(label)
        registerMask(label, mask)
    }

    x$referenceLabel <- c(x$referenceLabel, label)
    label <- getLabelID(label)
    x <- garnishGrob(x, "mask" = paste0("url(#", prefixName(label), ")"),
                     group = group)
    class(x) <- unique(c("masked.grob", class(x)))
    x
}

mask <- function(grob,
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

    mask <- list(grob = grob,
                 x = x, y = y,
                 width = width, height = height,
                 just = just, hjust = hjust, vjust = vjust)
    class(mask) <- "mask"
    mask
}

registerMask <- function(label, mask = NULL, ...) {
    checkExistingDefinition(label)
    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)
    
    if (is.null(mask)) {
        mask <- gridSVG::mask(...)
    } else if (! inherits(mask, "mask")) {
        stop("'mask' must be a 'mask' object")
    }

    if (is.null(mask$grob))
        stop("A grob must be given for a mask definition")

    # Now convert *at time of definition* to absolute units (inches)
    loc <- leftbottom(mask$x, mask$y, mask$width, mask$height, 
                      mask$just, mask$hjust, mask$vjust, NULL)
    x <- loc$x
    y <- loc$y
    width <- convertWidth(mask$width, "inches")
    height <- convertHeight(mask$height, "inches")

    defList <- list(label = label,
                    id = getID(label, "ref"),
                    x = x,
                    y = y,
                    width = width,
                    height = height,
                    grob = mask$grob)
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

primToDev.mask <- function(x, dev) {
    devStartMaskGroup(list(name = getID(x$name, "grob"),
                           mask = getLabelID(x$label)), NULL, dev)
}

drawDef.maskDef <- function(x, dev) {
    grob <- x$grob
    # This is always going to be true because we basically assume that
    # referenced content is fixed and therefore the names don't really
    # matter.
    if (get("use.gPaths", envir = .gridSVGEnv))
        grob$name <- paste(x$label, grob$name,
                           sep = getSVGoption("gPath.sep"))
    # Start mask
    devStartMask(devGrob(x, dev), NULL, dev)
    # Draw grob
    grobToDev(grob, dev)
    # Close mask
    devEndMask(devGrob(x, dev), NULL, dev)
}


devGrob.maskDef <- function(x, dev) {
    list(x=cx(x$x, dev),
         y=cy(x$y, dev),
         width=cw(x$width, dev),
         height=ch(x$height, dev),
         name=x$id)
}

svgStartMaskGroup <- function(id = NULL, mask = NULL,
                              svgdev = svgDevice()) {
    maskID <- paste0("url(#", prefixName(mask), ")")
    m <- newXMLNode("g", attrs = list(id = prefixName(id),
                                      mask = maskID),
                     parent = svgDevParent(svgdev))
    svgDevChangeParent(m, svgdev)
}

svgStartMask <- function(id = NULL, x=0, y=0, width=0, height=0,
                         svgdev = svgDevice()) {
    mask <- newXMLNode("mask", attrs = list(id = prefixName(id),
                                            x = round(x, 2), y = round(y, 2),
                                            width = round(width, 2),
                                            height = round(height, 2),
                                            maskUnits = "userSpaceOnUse"),
                       parent = svgDevParent(svgdev))
    svgDevChangeParent(mask, svgdev)
}

svgEndMask <- function(svgdev = svgDevice()) {
    # Go up one levels from mask to defs
    svgDevChangeParent(xmlParent(svgDevParent(svgdev)), svgdev)
}

