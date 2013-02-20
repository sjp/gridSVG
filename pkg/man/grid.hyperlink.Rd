\name{grid.hyperlink}
\alias{grid.hyperlink}
\alias{hyperlinkGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Associate a hyperlink with a grid grob }
\description{
  Creates a linked.grob object which is a normal grid grob,
  with a hyperlink attached.  Useful in conjunction with
  \code{gridToSVG}, to produce an SVG document with
  hyperlinked graphical elements.
}
\synopsis{
hyperlinkGrob(x, href, show=NULL, group=TRUE)
grid.hyperlink(path, href, show=NULL, group=TRUE, grep=FALSE)
  }
\usage{
grid.hyperlink(path, href, show, group, grep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
  \item{grep}{ Whether the \code{path} should be treated as a regular
    expression. }
}
\value{
  A linked.grob object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{gridToSVG}} }
\keyword{ dplot }% at least one, from doc/KEYWORDS

