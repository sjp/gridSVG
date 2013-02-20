\name{Coordinate Conversion Functions}
\alias{viewportConvertX}
\alias{viewportConvertY}
\alias{viewportConvertWidth}
\alias{viewportConvertHeight}
\title{
  Functions for using an imported coordinate system
}
\description{
  These functions convert between different units. The conversion occurs
  within viewports unknown to \code{grid}, but imported to R via
  \code{\link{gridSVGCoords}}.
}
\usage{
viewportConvertX(vpname, x, from, to = "svg")
viewportConvertY(vpname, x, from, to = "svg")
viewportConvertWidth(vpname, x, from, to)
viewportConvertHeight(vpname, x, from, to)
}
\arguments{
  \item{vpname}{
    The name of the viewport that the unit belongs within.
  }
  \item{x}{
    The size of the unit in \code{from} units.
  }
  \item{from}{
    The type of unit that \code{x} is.
  }
  \item{to}{
    The unit that \code{x} is being converted to.
  }
}
\details{
  Although \code{grid} has conversion functions available, it
  only converts units relative to the current viewport. After writing
  out to SVG, we no longer have actual grid viewports to convert units
  within.

  These functions are designed so that once coordinate information is
  loaded into \code{gridSVG} via \code{\link{gridSVGCoords}}, we can
  translate units within each of these viewports. Note: this requires
  that a \code{gridSVG} plot has had viewport information exported.

  These functions can be used in much the same way as \code{grid}'s unit
  conversion functions, the only difference being that we have a new
  unit, \code{svg}, which represents the size of a unit in SVG pixels.
}

\value{
  A numeric vector containing a single value, the value of the new unit.

  In the case of the \code{viewportConvertX} and \code{viewportConvertY}
  functions, we always return a value that is in terms of SVG pixels.
}
\author{
  Simon Potter
}
