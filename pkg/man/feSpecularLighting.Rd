\name{feSpecularLighting}
\alias{feSpecularLighting}
\title{
  Light an image using the alpha channel as a bump map.
}
\description{
  This filter primitive lights a source graphic using the alpha channel
  as a bump map. The resulting image is an RGBA image based on the light
  colour. The lighting calculation follows the standard specular
  component of the Phong lighting model. The resulting image depends on
  the light colour, light position and surface geometry of the input bump
  map. The result of the lighting calculation is added. The filter
  primitive assumes that the viewer is at infinity in the z direction
  (i.e., the unit vector in the eye direction is (0,0,1) everywhere).

  This filter primitive produces an image which contains the specular
  reflection part of the lighting calculation. Such a map is intended to
  be combined with a texture using the add term of the arithmetic method
  in \code{\link{feComposite}}. Multiple light sources can be simulated
  by adding several of these light maps before applying it to the
  texture image.
}
\usage{
feSpecularLighting(input = NA,
                   surfaceScale = 1, specularConstant = 1,
                   specularExponent = 1, kernelUnitLength = NA,
                   lightSource = NULL, ...)
}
\arguments{
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{surfaceScale}{
    Scale applied to the input alpha surface.
  }
  \item{specularConstant}{
    \code{kd} in the Phong lighting model. Must be non-negative.
  }
  \item{specularExponent}{
    Numeric exponent for specular term, larger is more "shiny". Range
    [1,128].
  }
  \item{kernelUnitLength}{
    The first number is the \code{dx} value. The second number is the
    \code{dy} value. If the \code{dy} value is not specified, it
    defaults to the same value as \code{dx}. Indicates the intended
    distance in current filter units (i.e., units as determined by the
    value of parent filter container's \code{primitiveUnits}) for
    \code{dx} and \code{dy}, respectively, in the surface normal
    calculation formulas. By specifying value(s) for
    \code{kernelUnitLength}, the kernel becomes defined in a scalable,
    abstract coordinate system. If \code{kernelUnitLength} is not
    specified, the \code{dx} and \code{dy} values should represent very
    small deltas relative to a given (x,y) position, which might be
    implemented in some cases as one pixel in the intermediate image
    offscreen bitmap, which is a pixel-based coordinate system, and thus
    potentially not scalable.
  }
  \item{lightSource}{
    A light source object, produced by one of
    \code{\link{feDistantLight}}, \code{\link{fePointLight}}, or
    \code{\link{feSpotLight}}.
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
  An \code{fe.specular.lighting} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feSpecularLightingElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}} \code{\link{fe}},
  \code{\link{feDistantLight}}, \code{\link{fePointLight}},
  \code{\link{feSpotLight}}.
}
