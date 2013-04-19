\name{feOffset}
\alias{feOffset}
\title{
  Offset an input image relative to its current position.
}
\description{
  This filter primitive offsets the input image relative to its current
  position in the image space by the specified vector.

  This is important for effects like drop shadows.
}
\usage{
feOffset(input = NA,
         dx = unit(0, "npc"), dy = unit(0, "npc"),
         default.units = "npc", ...)
}
\arguments{
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{dx}{
    The amount to offset \code{input} by along the x-axis.
  }
  \item{dy}{
    The amount to offset \code{input} by along the y-axis.
  }
  \item{default.units}{
    A string indicating the default units to use if \code{dx} or
    \code{dy} are only given as numeric vectors.
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
  An \code{fe.offset} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feOffsetElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
