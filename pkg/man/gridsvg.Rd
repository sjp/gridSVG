\name{gridsvg}
\alias{gridsvg}
\alias{dev.off}
\title{
  gridSVG Graphics Device
}
\description{
  Provides a convenient and familiar graphics device interface for the
  gridSVG package.
}
\usage{
gridsvg(name = "Rplots.svg",
        exportCoords = c("none", "inline", "file"),
        exportMappings = c("none", "inline", "file"),
        exportJS = c("none", "inline", "file"),
        res = NULL,
        indent = TRUE,
        htmlWrapper = FALSE,
        usePaths = c("vpPaths", "gPaths", "none", "both"),
        uniqueNames = TRUE,
        annotate = TRUE,
        xmldecl = xmlDecl(), ...)
dev.off(which = dev.cur())
}
\arguments{
  \item{name, exportCoords, exportMappings, exportJS, res, indent,
    htmlWrapper, usePaths, uniqueNames, annotate, xmldecl}{
    These parameters are passed onto \code{\link{grid.export}}.
  }
  \item{\dots}{
    Further parameters that are passed onto a \code{NULL}
    \code{\link{pdf}} graphics device. Useful parameters include
    \code{width} and \code{height}.
  }
  \item{which}{
    An integer specifying a device number.
  }
}
\details{
  These functions provide a more familiar and perhaps convenient
  interface to gridSVG than \code{\link{grid.export}}. It uses a PDF
  device as drawing occurs, but when the device needs to be written out
  (via \code{dev.off}) then it will save an SVG image instead.

  When a grid display list is not in use, or any device other than the
  \code{gridsvg} device is used, the behaviour of \code{dev.off} is the
  same as \code{\link[grDevices]{dev.off}} from the \code{grDevices}
  package.
}
\value{
  \code{gridsvg} returns nothing.

  \code{dev.off} will return in the same manner as
  \code{\link{grid.export}}. A list is always returned, but invisibly
  when an invalid filename is given.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{pdf}} and \code{\link{grid.export}}.
}
