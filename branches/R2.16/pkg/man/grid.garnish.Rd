\name{grid.garnish}
\alias{grid.garnish}
\alias{garnishGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Associate arbitrary SVG attributes with a grid grob }
\description{
  Creates an svg.grob object which is a normal grid grob,
  with SVG attributes attached.  Useful in conjunction with
  \code{gridToSVG}, to produce an SVG document with
  attributes that have no corresponding concept in grid graphics.
}
\usage{
garnishGrob(x, ..., group=TRUE)
grid.garnish(path, ..., grep=FALSE, redraw=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A grob.}
  \item{path}{ A grob path specifying a drawn grob. }
  \item{...}{ Arbitrary SVG attribute settings. }
  \item{group}{ A logical indicating whether the SVG attributes
    should be attached to the overall parent group for the
    relevant SVG element, or to individual SVG elements. }
  \item{grep}{A logical indicating whether to treat the \code{path}
    as a regular expression.}
  \item{redraw}{A logical indicating whether to redraw the current page.}
}
\details{
  The SVG attribute settings can be vectors (in the case of garnishing
  individual SVG elements) or even named vectors (if you want precise
  control over which attribute value is apportioned to which
  individual SVG element).
}
\value{
  A garnished.grob object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{gridToSVG}} }                           
\keyword{ dplot }% at least one, from doc/KEYWORDS

