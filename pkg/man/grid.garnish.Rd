\name{grid.garnish}
\alias{grid.garnish}
\alias{garnishGrob}
\title{ Associate arbitrary SVG attributes with a grid grob }
\description{
  Creates an svg.grob object which is a normal grid grob,
  with SVG attributes attached.  Useful in conjunction with
  \code{grid.export}, to produce an SVG document with
  attributes that have no corresponding concept in grid graphics.
}
\usage{
garnishGrob(x, ..., group=TRUE)
grid.garnish(path, ..., group=TRUE, redraw=FALSE,
             strict = FALSE, grep=FALSE, global=FALSE)
}
\arguments{
  \item{x}{A grob.}
  \item{path}{ A grob path specifying a drawn grob. }
  \item{...}{ Arbitrary SVG attribute settings. }
  \item{group}{ A logical indicating whether the SVG attributes
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
\seealso{ \code{\link{grid.export}} }
\keyword{ dplot }
