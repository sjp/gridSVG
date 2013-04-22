\name{Gradient Stops}
\alias{addGradientStop}
\alias{gradientStop}
\title{
  Definition and use of Gradient Stops.
}
\description{
  The ramp of colours to use on a gradient are defined by gradient
  stops. These gradient stops need to be added to a gradient object to
  produce a gradient.
}
\usage{
gradientStop(col, offset = 0)
addGradientStop(gradient, gradientStop, after = NA)
}
\arguments{
  \item{col}{
    The colour used for the gradient stop.
  }
  \item{offset}{
    Numeric. The point at which the colour \code{col} starts within the
    gradient. Must be between 0 and 1 (equivalent to \code{npc} units).
  }
  \item{gradient}{
    A gradient object to add a gradient stop to.
  }
  \item{gradientStop}{
    A gradient stop object.
  }
  \item{after}{
    The position that the gradient stop will inserted after. \code{NA}
    means to insert the gradient stop after the last position.
  }
}
\details{
  Each gradient stop is only positioned (with its offset) relative to
  the gradient itself.

  The order in which the gradient stops are added to the gradient is
  important. Gradient stops which appear earlier are drawn first, with
  later stops being drawn over the top.
}
\value{
  For \code{gradientStop}, a \code{gradient.stop} object.

  For \code{addGradientStop}, the modified gradient object.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{linearGradient}}, \code{\link{radialGradient}},
  \code{\link{grid.gradientFill}}
}
