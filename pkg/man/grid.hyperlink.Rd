\name{grid.hyperlink}
\alias{grid.hyperlink}
\alias{hyperlinkGrob}
\title{ Associate a hyperlink with a grid grob }
\description{
  Creates a linked.grob object which is a normal grid grob,
  with a hyperlink attached.  Useful in conjunction with
  \code{grid.export}, to produce an SVG document with
  hyperlinked graphical elements.
}
\usage{
grid.hyperlink(path, href, show=NULL, group=TRUE, redraw=FALSE,
               strict=FALSE, grep=FALSE, global=FALSE)
hyperlinkGrob(x, href, show=NULL, group=TRUE)
}
\arguments{
  \item{x}{ A grob to add a hyperlink to. }
  \item{path}{ A grob path specifying a drawn grob. }
  \item{href}{ A valid Xlink URI.  Can be a vector of several links
    (see \code{group} argument below). }
  \item{show}{
    A character vector specifying how the link should be opened.
    \code{NULL} and \code{""} will avoid adding an attribute. The most
    common cases are to use \code{"new"} to open a link in a new
    window/tab, or \code{"replace"} to open the link in the current
    window/tab.
  }
  \item{group}{ A logical indicating whether the hyperlinks
    should be attached to the overall parent group for the
    relevant SVG element, or to individual SVG elements. }
  \item{redraw}{
    A logical value to indicate whether to redraw the grob.
  }
  \item{strict}{
    A boolean indicating whether the \code{path} must be matched
    exactly.
  }
  \item{grep}{
    Whether the \code{path} should be treated as a regular expression.
  }
  \item{global}{
    A boolean indicating whether the function should affect just the
    first match of the \code{path}, or whether all matches should be
    affected.
  }
}
\value{
  A linked.grob object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.export}} }
\keyword{ dplot }
