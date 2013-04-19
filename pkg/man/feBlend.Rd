\name{feBlend}
\alias{feBlend}
\title{
  Blend two objects together.
}
\description{
  This filter composites two objects together using commonly used
  imaging software blending modes. It performs a pixel-wise combination
  of two input images.
}
\usage{
feBlend(input1 = NA, input2 = NA,
        mode = c("normal", "multiply", "screen", "darken", "lighten"),
        ...)
}
\arguments{
  \item{input1}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{input2}{
    Identfies a second input for this filter primitive. See
    \code{\link{filterInputs}}.
  }
  \item{mode}{
    An image blending mode.
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
  An \code{fe.blend} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feBlendElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
