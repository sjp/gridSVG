\name{feTurbulence}
\alias{feTurbulence}
\title{
  Create an image using the Perlin turbulence function.
}
\description{
  This filter primitive creates an image using the Perlin turbulence
  function. It allows the synthesis of artificial textures like clouds
  or marble.
}
\usage{
feTurbulence(baseFrequency = 0, numOctaves = 1,
             seed = 1, stitchTiles = FALSE,
             type = c("turbulence", "fractalNoise"), ...)
}
\arguments{
  \item{baseFrequency}{
    The base frequency (frequencies) parameter(s) for the noise
    function. If a two element numeric vector is provided, the first
    number represents a base frequency in the X direction and the second
    value represents a base frequency in the Y direction. If one number
    is provided, then that value is used for both X and Y.
  }
  \item{numOctaves}{
    The \code{numOctaves} parameter for the noise function.
  }
  \item{seed}{
    The starting number for the pseudo random number generator.
  }
  \item{stitchTiles}{
    If \code{stitchTiles} is \code{FALSE}, no attempt it made to
    achieve smooth transitions at the border of tiles which contain a
    turbulence function. Sometimes the result will show clear
    discontinuities at the tile borders.

    If \code{stitchTiles} is \code{TRUE}, then the user agent will
    automatically adjust \code{baseFrequency}-x and
    \code{baseFrequency}-y values such that the \code{feTurbulence}'s
    width and height (i.e., the width and height of the current
    subregion) contains an integral number of the Perlin tile width and
    height for the first octave. The \code{baseFrequency} will be
    adjusted up or down depending on which way has the smallest relative
    (not absolute) change as follows: Given the frequency, calculate
    \eqn{lowFreq = \lfloor width * frequency \rfloor / width}{lowFreq =
    floor(width*frequency)/width} and \eqn{hiFreq = \lceil
    width*frequency \rceil
    /width}{hiFreq=ceil(width*frequency)/width}. If
    \eqn{frequency/lowFreq < hiFreq/frequency}{frequency/lowFreq <
    hiFreq/frequency} then use \code{lowFreq}, else use
    \code{hiFreq}. While generating turbulence values, generate lattice
    vectors as normal for Perlin Noise, except for those lattice points
    that lie on the right or bottom edges of the active area (the size
    of the resulting tile). In those cases, copy the lattice vector from
    the opposite edge of the active area.
  }
  \item{type}{
    Indicates whether the filter primitive should perform a noise or
    turbulence function.
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
  An \code{fe.turbulence} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feTurbulenceElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
