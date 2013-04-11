
# Functions to take a grid grob and call appropriate
# functions from svg.R to produce SVG output


# User function
gridToSVG <- function(name="Rplots.svg",
                      export.coords=c("none", "inline", "file"),
                      export.mappings=c("none", "inline", "file"),
                      export.js=c("none", "inline", "file"),
                      res = NULL,
                      prefix = "",
                      indent = TRUE,
                      htmlWrapper = FALSE,
                      usePaths = c("vpPaths", "gPaths", "none", "both"),
                      uniqueNames = TRUE,
                      annotate = TRUE,
                      xmldecl = xmlDecl()) {
    # 'XML' can sometimes give us namespace warnings, despite producing
    # valid SVG. Silence any warnings that 'XML' might give us.
    if (! is.null(getOption("gridSVGWarnings")) &&
        ! getOption("gridSVGWarnings")) {
        oldNSWarning <- options(suppressXMLNamespaceWarning = TRUE)
        on.exit(options(suppressXMLNamespaceWarning =
                        oldNSWarning$suppressXMLNamespaceWarning))
    }

    # Important to know if we need to modify vpPaths/gPaths at all
    usePaths <- match.arg(usePaths)
    paths <-
        if (usePaths == "vpPaths")
            c(TRUE, FALSE)
        else if (usePaths == "gPaths")
            c(FALSE, TRUE)
        else if (usePaths == "both")
            rep(TRUE, 2)
        else # Assume "none"
            rep(FALSE, 2)
    assign("use.vpPaths", paths[1], envir = .gridSVGEnv)
    assign("use.gPaths", paths[2], envir = .gridSVGEnv)
    assign("uniqueNames", uniqueNames, envir = .gridSVGEnv)
    assign("prefix", prefix, envir = .gridSVGEnv)

    # Saving how to export
    export.coords <- match.arg(export.coords)
    export.mappings <- match.arg(export.mappings)
    export.js <- match.arg(export.js)
    # If we are exporting js but returning a character
    # vector we need to save the contents inline, because
    # we don't want to touch the disk
    if (is.null(name) || ! nzchar(name)) {
        if (export.coords == "file") {
            export.coords <- "inline"
            warning('export.coords changed from "file" to "inline"')
        }
        if (export.mappings == "file") {
            export.mappings <- "inline"
            warning('export.mappings changed from "file" to "inline"')
        }
        if (export.js == "file") {
            export.js <- "inline"
            warning('export.js changed from "file" to "inline"')
        }
    }
    assign("export.coords", export.coords, envir = .gridSVGEnv)
    assign("export.mappings", export.mappings, envir = .gridSVGEnv)
    assign("export.js", export.js, envir = .gridSVGEnv)

    # Ensure we're at the top level
    upViewport(0)
    rootgp <- get.gpar()
    rootvp <- current.viewport()
    roottm <- current.transform()

    svgdev <- openSVGDev(name, width=par("din")[1], height=par("din")[2], res = res)
    # grid.force() the scene to resolve high-level grobs
    # to their standard components
    grid.force(redraw = FALSE)
    # Create a gTree from the current page
    # NOTE that set the 'gp' slot on this top-level gTree
    # based on ROOT vp
    # Use 'wrap=TRUE' to ensure correct capture of all types of 'grid' output
    gTree <- grid.grab(name="gridSVG", wrap=TRUE, gp=rootgp)
    if (anyRefsDefined()) {
        # Reducing only to reference definitions
        usageTable <- get("usageTable", envir = .gridSVGEnv)
        usageTable <- usageTable[usageTable$type == "ref", ]
        assign("usageTable", usageTable, envir = .gridSVGEnv)
    } else {
        # Emptying the usage table
        assign("usageTable",
               data.frame(name = character(0),
                         suffix = integer(0),
                         type = character(0),
                         selector = character(0),
                         xpath = character(0),
                         stringsAsFactors = FALSE),
               envir = .gridSVGEnv)
    }
    # Emptying point usage table
    assign("pchUsageTable", 
           matrix(c(0:127, logical(128)), ncol = 2,
                  dimnames = list(NULL, c("pch", "used"))),
           envir = .gridSVGEnv)
    # Because the root viewport is never entered into, we need to set
    # the root vp coordinate information before we start entering into
    # other VPs
    currVpCoords <- list(ROOT = getCoordsInfo(rootvp, roottm, svgdev))
    assign("vpCoords", currVpCoords, envir = .gridSVGEnv)
    # When using referenced content, the ID generated at the time of
    # definition may be different to the ID at draw time, see getSVGoptions()
    assignRefIDs()
    # Convert gTree to SVG
    gridToDev(gTree, svgdev)
    # Flush out any referenced definitions so that grobs can use them
    flushDefinitions(svgdev)
    svgroot <- devClose(svgdev)
    # Adding in JS if necessary, always write utils *last*.
    # Not strictly necessary but may avoid potential issues in JS.
    coords <- svgCoords(export.coords, name, svgroot)
    mappings <- svgMappings(export.mappings, name, svgroot)
    jsutils <- svgJSUtils(export.js, name, svgroot)
    # If we're annotating output with gridSVG call info
    if (annotate) {
        # Realise true values for some arguments
        if (is.null(name))
            name <- ""
        if (is.null(res))
            res <- round(par("cra")[1] / par("cin")[1], 2)
        # Ignore annotate in this list, because it will be implied
        # Also ignoring the XML declaration, we can see it in the
        # output directly.
        callAttrs <- list(
            name = name,
            export.coords = export.coords,
            export.mappings = export.mappings,
            export.js = export.js,
            res = res,
            prefix = prefix,
            indent = indent,
            htmlWrapper = htmlWrapper,
            usePaths = usePaths,
            uniqueNames = uniqueNames
        )
        svgAnnotate(svgroot, callAttrs)
    }

    doctxt <- saveXML(svgroot, indent = indent)

    # In an on-screen device, we can be left with a blank device
    # so refresh just to ensure we can see everything. Also happens
    # with devices like png and pdf so just force a refresh.
    # Also, to avoid having to ask to refresh, just temporarily
    # disable asking.
    old.ask <- devAskNewPage(FALSE)
    on.exit(devAskNewPage(old.ask), add = TRUE)
    grid.refresh()

    # See if we need an XML declaration added
    if (! is.null(xmldecl))
        doctxt <- paste0(xmldecl, doctxt)

    result <- list(svg = svgroot,
                   coords = coords,
                   mappings = mappings,
                   utils = jsutils)

    if (! testUniqueMappings(svgroot))
        warning("Not all element IDs are unique. Consider running 'gridToSVG' with 'uniqueNames = TRUE'.")

    # Return SVG list when an inadequate filename is supplied
    if (is.null(name) || ! nzchar(name))
        return(result)

    # Save SVG
    cat(doctxt, file = name)
    # Write an HTML wrapper for this
    if (htmlWrapper)
        htmlFile(name, svgdev@dev)

    # Return result invisibly
    invisible(result)
}

gridSVG.newpage <- function(wipeRefs = TRUE, recording = TRUE) {
    if (wipeRefs) {
        assign("refDefinitions", list(), envir = .gridSVGEnv)
        assign("refUsageTable",
               data.frame(label = character(0),
                          used = logical(0),
                          stringsAsFactors = FALSE),
               envir = .gridSVGEnv)
        assign("usageTable",
               data.frame(name = character(0),
                         suffix = integer(0),
                         type = character(0),
                         selector = character(0),
                         xpath = character(0),
                         stringsAsFactors = FALSE),
               envir = .gridSVGEnv)
    }
    grid.newpage(recording = recording)
}

gridsvg <- function(name="Rplots.svg",
                    export.coords=c("none", "inline", "file"),
                    export.mappings=c("none", "inline", "file"),
                    export.js=c("none", "inline", "file"),
                    res = NULL,
                    indent = TRUE,
                    htmlWrapper = FALSE,
                    usePaths = c("vpPaths", "gPaths", "none", "both"),
                    uniqueNames = TRUE,
                    annotate = TRUE,
                    xmldecl = xmlDecl(),
                    ...) {
    callargs <- as.list(match.call(expand.dots = FALSE))[-1]
    dev.args <- as.list(callargs$`...`) # pairlists...
    if (is.null(dev.args))
        dev.args <- list(file = NULL)
    else
        dev.args["file"] <- list(NULL)
    do.call("pdf", dev.args)
    devind <- which(names(callargs) == "...")
    gridsvg.args <- if (length(devind)) callargs[-devind]
                    else callargs
    if (exists("gridToSVGArgs", envir = .gridSVGEnv))
        gridToSVGArgs <- get("gridToSVGArgs", envir = .gridSVGEnv)
    else
        gridToSVGArgs <- list()
    gridToSVGArgs[[dev.cur()]] <- gridsvg.args
    assign("gridToSVGArgs", gridToSVGArgs, envir = .gridSVGEnv)
    # HACK!
    # This renames the pdf device to "gridsvg" purely for convenience.
    devs <- .Devices
    devs[[dev.cur()]] <- "gridsvg"
    assign(".Devices", devs, envir = baseenv())
}

dev.off <- function(which = dev.cur()) {
    if (.Devices[[which]] == "gridsvg") {
        # If there's nothing on the display list then nothing
        # can be drawn
        if (! length(grid.ls(print = FALSE)$name)) {
            grDevices::dev.off(which)
            warning("No grid image was drawn so no SVG was created")
            return(invisible())
        }
        gridsvg.args <- get("gridToSVGArgs", envir = .gridSVGEnv)[[which]]
        name <- gridsvg.args$name
        image <- do.call("gridToSVG", gridsvg.args)
        grDevices::dev.off(which)
        if (is.null(name) || ! nzchar(name))
            image
        else
            invisible(image)
    } else {
        grDevices::dev.off(which)
    }
}
