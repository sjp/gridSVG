\name{fePointLight}
\alias{fePointLight}
\title{
  Create a Point Light Source
}
\description{
  This filter primitive defines a point light source that can be used
  within a lighting filter primitive: \code{\link{feDiffuseLighting}} or
  \code{\link{feSpecularLighting}}.
}
\usage{
fePointLight(z = unit(0, "npc"), default.units = "npc", zdim = "x", ...)
}
\arguments{
  \item{z}{
    A numeric vector or unit object specifying z-location.
  }
  \item{default.units}{
    A string indicating the default units to use if \code{z} if given as
    a numeric vector.
  }
  \item{zdim}{
    Either \code{"x"} or \code{"y"}. Determines the dimension to which
    \code{z} will be located relative to. This is necessary because R
    graphics has no concept of a z-dimension.
  }
  \item{\dots}{
    Further arguments to be passed onto \code{fe}.
  }
}
\details{
  For more information about this primitive, consult the reference to
  the SVG specification.
}
\value{
  An \code{fe.point.light} object.
}
\references{
  \code{http://www.w3.org/TR/SVG/filters.html#fePointLightElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}},
  \code{\link{feDiffuseLighting}}, \code{\link{feSpecularLighting}}.
}
