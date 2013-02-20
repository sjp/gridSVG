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
hyperlinkGrob(x, href, group=TRUE)
grid.hyperlink(path, href, group=TRUE)
  }
\usage{
grid.hyperlink(path, href, group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{ A grob path specifying a drawn grob. }
  \item{href}{ A valid Xlink URI.  Can be a vector of several links
    (see \code{group} argument below). }
  \item{group}{ A logical indicating whether the hyperlinks
    should be attached to the overall parent group for the
    relevant SVG element, or to individual SVG elements. }
}
\value{
  A linked.grob object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{gridToSVG}} }                           
\keyword{ dplot }% at least one, from doc/KEYWORDS

