# High level functions for applying clipping paths
grid.clipPath <- function(...) {
    grid.draw(clipPathGrob(...))
}

clipPathGrob <- function(grob, name = NULL) {
    grid::grob(grob = grob, name = name, cl = "clipPath")
}

primToDev.clipPath <- function(x, dev) {
    grob <- x$grob
    if (get("use.gPaths", envir = .gridSVGEnv))
        grob$name <- paste(x$name, grob$name,
                           sep = getSVGoption("gPath.sep"))
    # Start defs, clipPath
    devStartClipPath(devGrob(x, dev), NULL, dev)
    # Draw grob
    grobToDev(grob, dev)
    # Close defs, clipPath, open group
    devEndClipPath(devGrob(x, dev), NULL, dev)
}

devGrob.clipPath <- function(x, dev) {
    list(name = x$name)
}

svgStartGrobClipPath <- function(id = NULL, svgdev = svgDevice()) {
    clipPathID <- prefixName(paste(id, "clipPath",
                                   sep = getSVGoption("id.sep")))
    defs <- newXMLNode("defs", parent = svgDevParent(svgdev))
    cp <- newXMLNode("clipPath", attrs = list(id = clipPathID),
                     parent = defs)
    svgDevChangeParent(cp, svgdev)
}

svgEndGrobClipPath <- function(svgdev = svgDevice()) {
    # First need to collect all children and filter out unwanted content
    defs <- svgDevParent(svgdev)
    clippath <- xmlChildren(defs)[[1]]
    nodelist <- flattenClippedSVG(clippath)
    # Wipe out all children, then add in the ones we want
    removeChildren(clippath, kids = xmlChildren(clippath))
    xmlChildren(clippath) <- nodelist

    # Go up two levels from clipPath to defs to parent
    svgDevChangeParent(xmlParent(xmlParent(svgDevParent(svgdev))), svgdev)
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

# High level functions for applying masks to grobs
grid.mask <- function(...) {
    grid.draw(maskGrob(...))
}

maskGrob <- function(grob,
                     x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                     width = unit(1, "npc"), height = unit(1, "npc"),
                     just = "centre", hjust = NULL, vjust = NULL,
                     default.units = "npc", name = NULL, vp = NULL) {
    if (! is.unit(x))
        x <- unit(x, default.units)
    if (! is.unit(y))
        y <- unit(y, default.units)
    if (! is.unit(width))
        width <- unit(width, default.units)
    if (! is.unit(height))
        height <- unit(height, default.units)
    grid::grob(grob = grob, x = x, y = y, width = width, height = height,
               just = just, hjust = hjust, vjust = vjust,
               name = name, vp = vp, cl = "mask")
}

primToDev.mask <- function(x, dev) {
    grob <- x$grob
    if (get("use.gPaths", envir = .gridSVGEnv))
        grob$name <- paste(x$name, grob$name,
                           sep = getSVGoption("gPath.sep"))
    # Start defs, mask
    devStartMask(devGrob(x, dev), NULL, dev)
    # Draw grob
    grobToDev(grob, dev)
    # Close defs, mask, open group
    devEndMask(devGrob(x, dev), NULL, dev)
}

devGrob.mask <- function(x, dev) {
    lb <- leftbottom(x$x, x$y, x$width, x$height,
                     x$just, x$hjust, x$vjust, dev)
    dim <- dimToInches(x$width, x$height, dev)
    list(x=cx(lb$x, dev),
         y=cy(lb$y, dev),
         width=cw(dim$w, dev),
         height=ch(dim$h, dev),
         name=x$name)
}

svgStartMask <- function(id = NULL, x=0, y=0, width=0, height=0,
                         svgdev = svgDevice()) {
    maskID <- prefixName(paste(id, "mask",
                               sep = getSVGoption("id.sep")))
    defs <- newXMLNode("defs", parent = svgDevParent(svgdev))
    mask <- newXMLNode("mask", attrs = list(id = maskID,
                                            x = round(x, 2), y = round(y, 2),
                                            width = round(width, 2),
                                            height = round(height, 2),
                                            maskUnits = "userSpaceOnUse"),
                       parent = defs)
    svgDevChangeParent(mask, svgdev)
}

svgEndMask <- function(svgdev = svgDevice()) {
    # Go up two levels, from mask to defs to parent
    svgDevChangeParent(xmlParent(xmlParent(svgDevParent(svgdev))), svgdev)
}

