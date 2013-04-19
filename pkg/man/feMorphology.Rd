\name{feMorphology}
\alias{feMorphology}
\title{
  "Fatten" or "thin" artwork.
}
\description{
  This filter primitive performs "fattening" or "thinning" of
  artwork. It is particularly useful for fattening or thinning an alpha
  channel.
}
\usage{
feMorphology(input = NA, operator = c("erode", "dilate"),
             radius = unit(0, "npc"), default.units = "npc", ...)
}
\arguments{
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{operator}{
    A keyword indicating whether to erode (i.e., thin) or dilate
    (fatten) the source graphic, \code{input}.
  }
  \item{radius}{
    The radius (or radii) for the operation. If two values are provided,
    the first value represents a x-radius and the second value
    represents a y-radius. If one radius is provided, then that value is
    used for both xand y.
  }
  \item{default.units}{
    A string indicating the default units to use if \code{radius} is
    only given as a numeric vector.
  }
  \item{\dots}{
    Further arguments to be passed onto \code{\link{fe}}.
  }
}
\details{
  For more information about this primitive, consult the reference to
  the SVG specification.
}
\value{
  An \code{fe.morphology} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feMorphologyElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
