\name{feDistantLight}
\alias{feDistantLight}
\title{
  Create a Distant Light Source
}
\description{
  This filter primitive defines a distant light source that can be used
  within a lighting filter primitive: \code{\link{feDiffuseLighting}} or
  \code{\link{feSpecularLighting}}.
}
\usage{
feDistantLight(azimuth = 0, elevation = 0, ...)
}
\arguments{
  \item{azimuth}{
    Direction angle for the light source on the x-y plane (clockwise),
    in degrees from the x axis.
  }
  \item{elevation}{
    Direction angle for the light source from the x-y plane towards the z
    axis, in degrees. Note the positive z-axis points towards the viewer
    of the content.
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
  An \code{fe.distant.light} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feDistantLightElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}},
  \code{\link{feDiffuseLighting}}, \code{\link{feSpecularLighting}}.
}
