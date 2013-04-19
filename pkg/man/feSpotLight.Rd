\name{feSpotLight}
\alias{feSpotLight}
\title{
  Create a Spot Light Source
}
\description{
  This filter primitive defines a spot light source that can be used
  within a lighting filter primitive: \code{\link{feDiffuseLighting}} or
  \code{\link{feSpecularLighting}}.
}
\usage{
feSpotLight(x = unit(0, "npc"), y = unit(0, "npc"), z = unit(0, "npc"),
            pointsAtX = unit(1, "npc"), pointsAtY = unit(1, "npc"),
            pointsAtZ = unit(0, "npc"), zdim = "x",
            default.units = "npc", specularExponent = 1,
            limitingConeAngle = NA, ...)
}
\arguments{
  \item{x}{
    A numeric vector or unit object specifying the x-location of the
    light source.
  }
  \item{y}{
    A numeric vector or unit object specifying the y-location of the
    light source.
}
  \item{z}{
    A numeric vector or unit object specifying the z-location of the
    light source.
  }
  \item{pointsAtX}{
    A numeric vector or unit object specifying the x-location that the
    light points at.
  }
  \item{pointsAtY}{
    A numeric vector or unit object specifying the y-location that the
    light points at.
  }
  \item{pointsAtZ}{
    A numeric vector or unit object specifying the z-location that the
    light points at.
  }
  \item{zdim}{
    Either \code{"x"} or \code{"y"}. Determines the dimension to which
    \code{z} and \code{pointsAtZ} will be located relative to. This is
    necessary because R graphics has no concept of a z-dimension.
  }
  \item{default.units}{
    A string indicating the default units to use if \code{x}, \code{y},
    \code{z}, \code{pointsAtX}, \code{pointsAtY}, \code{pointsAtZ} are
    only given as numeric vectors.
  }
  \item{specularExponent}{
    Exponent value controlling the focus for the light source.
  }
  \item{limitingConeAngle}{
    If \code{NA}, no limiting cone is applied, otherwise a limiting cone
    which restricts the region where the light is projected. No light is
    projected outside the cone. \code{limitingConeAngle} represents the
    angle in degrees between the spot light axis (i.e. the axis between
    the light source and the point to which it is pointing at) and the
    spot light cone.
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
  An \code{fe.spot.light} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feSpotLightElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}},
  \code{\link{feDiffuseLighting}}, \code{\link{feSpecularLighting}}.
}
