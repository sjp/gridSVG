
# Functions to take a grid grob and call appropriate
# functions from svg.R to produce SVG output


# User function
gridToSVG <- function(name="Rplots.svg",
                      export.coords=c("file", "inline", "none"),
                      export.js=c("file", "inline", "none"),
                      res = NULL,
                      indent = TRUE,
                      xmldecl = xmlDecl()) {
    # Saving we know how to export
    export.coords <- match.arg(export.coords)
    export.js <- match.arg(export.js)
    # If we are exporting js but returning a character
    # vector we need to save the contents inline, because
    # we don't want to touch the disk
    if (is.null(name) || ! nzchar(name)) {
        if (export.coords == "file") {
            export.coords <- "inline"
            warning('export.coords changed from "file" to "inline"')
        }
        if (export.js == "file") {
            export.js <- "inline"
            warning('export.js changed from "file" to "inline"')
        }
    }
    assign("export.coords", export.coords, envir = .gridSVGEnv)
    assign("export.js", export.js, envir = .gridSVGEnv)

    # Ensure we're at the top level
    upViewport(0)
    rootgp <- get.gpar()
    rootvp <- current.viewport()
    roottm <- current.transform()

    svgdev <- openSVGDev(name, width=par("din")[1], height=par("din")[2], res = res)
    # grid.force() the scene to resolve high-level grobs
    # to their standard components
    grid.force()
    
    # Create a gTree from the current page
    # NOTE that set the 'gp' slot on this top-level gTree
    # based on ROOT vp
    # Use 'wrap=TRUE' to ensure correct capture of all types of 'grid' output
    gTree <- grid.grab(name="gridSVG", wrap=TRUE, gp=rootgp)
    # Emptying the VP usage table
    vpUsageTable <- data.frame(vpname = character(0),
                               count = integer(0),
                               stringsAsFactors=FALSE)
    assign("vpUsageTable", vpUsageTable, envir = .gridSVGEnv)
    # Emptying point usage table
    pchUsageTable <- matrix(c(0:127, logical(128)), ncol = 2,
                            dimnames = list(NULL, c("pch", "used")))
    assign("pchUsageTable", pchUsageTable, envir = .gridSVGEnv)
    # Because the root viewport is never entered into, we need to set
    # the root vp coordinate information before we start entering into
    # other VPs
    currVpCoords <- list(ROOT = getCoordsInfo(rootvp, roottm, svgdev))
    assign("vpCoords", currVpCoords, envir = .gridSVGEnv)

    # Convert gTree to SVG
    gridToDev(gTree, svgdev)
    svgroot <- devClose(svgdev)
    # Adding in JS if necessary, always write coords *first*
    # Not strictly necessary but may avoid potential issues
    coords <- svgCoords(export.coords, name, svgroot)
    jsutils <- svgJSUtils(export.js, name, svgroot)
    doctxt <- saveXML(svgroot, indent = indent)

    # In an on-screen device, we can be left with a blank device
    # so refresh just to ensure we can see everything. Also happens
    # with devices like png and pdf so just force a refresh.
    # Also, to avoid having to ask to refresh, just temporarily
    # disable asking.
    old.ask <- devAskNewPage(FALSE)
    on.exit(devAskNewPage(old.ask))
    grid.refresh()

    # See if we need an XML declaration added
    if (! is.null(xmldecl))
        doctxt <- paste0(xmldecl, doctxt)

    # Return SVG vector when an inadequate filename is supplied
    if (is.null(name) || ! nzchar(name))
        return(list(svg = svgroot,
                    coords = coords,
                    utils = jsutils))

    # Save SVG
    cat(doctxt, file = name)
    # Write an HTML wrapper for this
    htmlFile(name, svgdev@dev)
}

old.gridToSVG <- function(name="Rplots.svg") {
  svgdev <- openSVGDev(name, width=par("din")[1], height=par("din")[2])
  # Start a new page because we are going to be reproducing the
  # pushing and popping of viewports and this needs to be done
  # from scratch
  grid.newpage(recording=FALSE)
  # Traverse the grid display list producing
  # SVG equivalents of all grid output
  # This nastily peeks into the grid NAMESPACE to get the
  # display list (for now)
  lapply(grid:::grid.Call("L_getDisplayList"), gridToDev, svgdev)
  # Before closing, need to pop the top-level viewport
  # which is not possible in grid
  devEndGroup(svgdev)
  devClose(svgdev)
}
