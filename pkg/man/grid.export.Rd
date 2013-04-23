\name{grid.export}
\alias{grid.export}
\alias{gridToSVG}
\title{ Generate SVG output from a grid graphic }
\description{
  Produces an SVG version of the current grid page.
}
\usage{
grid.export(name = "Rplots.svg",
            export.coords = c("none", "inline", "file"),
            export.mappings = c("none", "inline", "file"),
            export.js = c("none", "inline", "file"),
            res = NULL,
            prefix = "",
            addClasses = FALSE,
            indent = TRUE,
            htmlWrapper = FALSE,
            usePaths = c("vpPaths", "gPaths", "none", "both"),
            uniqueNames = TRUE,
            annotate = TRUE,
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
  \item{export.mappings}{
    If this parameter is not \code{none} a mapping file is exported.

    If this parameter is \code{file}, the mapping information is written
    to a file, while \code{inline} will include the contents within the
    SVG document.
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
  \item{prefix}{
    A prefix to apply to all generated SVG ID attributes. Useful for
    ensuring unique IDs when many SVG images exist within the same HTML
    document.

    If a valid prefix has been given, the root \code{<svg>} element will
    be given an ID attribute with the prefix as its value.
  }
  \item{addClasses}{
    If \code{TRUE}, adds an SVG class attribute to all grobs and
    viewports which holds the value of the class of the grob or
    viewport. If the class attribute already exists (via
    \code{\link{grid.garnish}} or \code{\link{grid.element}}), the
    resulting SVG class attribute will be the union of the existing
    class attribute and the grob/viewport classes.
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
  \item{usePaths}{
    If this parameter is set to \code{vpPaths}, then when writing out
    viewports gridSVG will set the SVG element ID to the current
    \code{vpPath} instead of the current viewport name.

    If this parameter is set to \code{gPaths}, gridSVG will set the
    names of grobs to be the current \code{gPath} instead of the current
    grob name.

    When \code{none}, viewports and grobs will not incorporate paths.

    When \code{both}, viewports and grobs will both use paths.
  }
  \item{uniqueNames}{
    If \code{TRUE}, gridSVG will make an attempt to produce unique grob
    names. Unique \code{id} attributes are required for valid SVG. It is
    highly recommended that mapping information is used when this
    parameter is \code{TRUE}.
  }
  \item{annotate}{
    If \code{TRUE}, an SVG metadata element will be introduced directly
    below the root \code{<svg>} element. This element contains XML that
    describes the information that gridSVG used to draw the image
    (mostly arguments to \code{grid.export}). This output may be useful
    for debugging purposes.
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
  The \code{uniqueNames} parameter is set to \code{TRUE} by default in
  order to ensure that each SVG element ID is unique. This is a
  requirement of XML (which SVG is based on). This differs from
  \code{usePaths} because \code{usePaths} can still generate names that
  are not unique (there are several ways for this to
  happen). \code{uniqueNames} modifies grob and viewport names with a
  numeric suffix to ensure uniqueness. When \code{FALSE}, only grob
  names will be kept unmodified because modifying viewport names would
  affect coordinate information.

  Occasionally the XML package can report warnings, despite valid SVG
  being produced. If spurious warnings are being produced, set
  \code{options(gridSVGWarnings = FALSE)} to ignore them.

  See the files in the directory gridSVG/tests for examples
  of things that can be done.  See the file gridSVG/doc/overview.tex
  for limitations.
}
\value{
  When \code{name} has a valid filename the side effect is to produce an
  SVG file of the specified name.

  Optionally a JavaScript file containing coordinate transformation
  information is also exported.

  Optionally a JavaScript file containing name mapping information is
  also exported.

  Optionally a JavaScript file containing utility JavaScript functions
  is also exported.

  When \code{name} has a filename with zero characters, a named list is
  returned with four elements. \code{svg} is the SVG root node (and all
  its children, see the XML package for more information on how to use
  this. \code{coords} contains the list of coordinate information for
  exported viewports. \code{mappings} is a list containing information
  on how names have been modified during the exporting process.
  \code{utils} is a character vector containing JavaScript code to
  manipulate gridSVG plots in the browser.

  This list is always returned but when a valid filename is given, it is
  returned invisibly.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.hyperlink}}, \code{\link{grid.animate}}, \code{\link{grid.garnish}} }
\keyword{ dplot }
