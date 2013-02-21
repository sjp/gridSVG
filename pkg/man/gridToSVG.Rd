\name{gridToSVG}
\alias{gridToSVG}
\title{ Generate SVG output from a grid graphic }
\description{
  Produces an SVG version of the current grid page.
}
\usage{
gridToSVG(name = "Rplots.svg",
          export.coords = c("file", "inline", "none"),
          export.js = c("file", "inline", "none"),
          res = NULL,
          indent = TRUE,
          htmlWrapper = FALSE,
          xmldecl = xmlDecl())
}
\arguments{
  \item{name}{
    The name of the SVG file to produce.

    If this parameter is \code{NULL} or \code{""}, a list
    containing the SVG document, coordinate information, and JavaScript
    utility functions are returned.
  }
  \item{export.coords}{
    If this parameter is not \code{none} a coordinates file is exported.

    If this parameter is \code{file}, the coordinates information is
    written to a file, while \code{inline} will include the contents
    within the SVG document.
  }
  \item{export.js}{
    If this parameter is not \code{none} a JavaScript file is written out.
    This contains useful functions for manipulating gridSVG plots in the
    browser, including unit conversion functions.

    If this parameter is \code{file}, the JavaScript file is written to
    a file, while \code{inline} will include the contents within the SVG
    document.
  }
  \item{res}{
    The device resolution to print at (in DPI).

    If \code{NULL}, this is automatically calculated to be the
    resolution of the current device. Typically the PDF device would be
    used, and this uses a resolution of 72, i.e. 72 DPI.
  }
  \item{indent}{
    Determines whether the resulting SVG document will be exported with
    indentation present.

    Indentation makes the document more readable, but when \code{indent}
    is set to \code{FALSE}, parsing the SVG in JavaScript is easier
    because there are no empty text nodes.
  }
  \item{htmlWrapper}{
    If \code{TRUE}, saves a wrapping HTML file. This file contains a
    snippet of HTML which links to the exported SVG file.
  }
  \item{xmldecl}{
    This parameter sets the XML declaraction that will be applied to the
    SVG document.

    By default this parameter simply declares that the document is XML
    version 1.0, along with the character encoding that was used to
    export the SVG document.

    If \code{xmldecl} is \code{NULL}, then no XML declaration is
    printed. This may be useful when you want only the SVG document and
    nothing more.
  }
}
\details{
  See the files in the directory gridSVG/tests for examples
  of things that can be done.  See the file gridSVG/doc/overview.tex
  for limitations.
}
\value{
  When \code{name} has a valid filename the side effect is to produce an
  SVG file of the specified name.

  Optionally a JavaScript file containing coordinate transformation
  information is also exported.

  Optionally a JavaScript file containing utility JavaScript functions
  is also exported.

  When \code{name} has a filename with zero characters, a named list is
  returned with three elements. \code{svg} is the SVG root node (and all
  its children, see the XML package for more information on how to use
  this. \code{coords} contains the list of coordinate information for
  exported viewports. \code{utils} is a character vector containing
  JavaScript code to manipulate gridSVG plots in the browser.

  This list is always returned but when a valid filename is given, it is
  returned invisibly.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.hyperlink}}, \code{\link{grid.animate}} }
\keyword{ dplot }
