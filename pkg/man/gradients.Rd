\name{Gradient Objects}
\alias{linearGradient}
\alias{radialGradient}
\title{
  Create Linear and Radial Gradients
}
\description{
  Create objects which describe linear and radial gradients. These
  objects can later be used to apply a gradient fill to grobs, and also
  to define a gradient so that it may be reused multiple times.
}
\usage{
linearGradient(col = c("black", "white"),
               stops = seq(0, 1, length.out = length(col)),
               gradientUnits = c("bbox", "coords"),
               x0 = unit(0, "npc"), x1 = unit(1, "npc"),
               y0 = unit(0, "npc"), y1 = unit(1, "npc"),
               default.units = "npc",
               spreadMethod = c("pad", "reflect", "repeat"))
radialGradient(col = c("black", "white"),
               stops = seq(0, 1, length.out = length(col)),
               gradientUnits = c("bbox", "coords"),
               x = unit(0.5, "npc"), y = unit(0.5, "npc"),
               r = unit(0.5, "npc"),
               fx = unit(0.5, "npc"), fy = unit(0.5, "npc"),
               default.units = "npc",
               spreadMethod = c("pad", "reflect", "repeat"))
}
\arguments{
  \item{col}{
    A vector of colours used for gradient stops.
  }
  \item{stops}{
    A numeric vector of offsets (typically between 0 and 1) to place the
    the colours (\code{col}) at.
  }
  \item{gradientUnits}{
    If \code{"bbox"}, the gradient is positioned relative to the
    bounding box of the referring grob. All units attempt to be
    converted to equivalent "npc" coordinates as a result.

    If \code{"coords"}, uses grid coordinates to determine positioning.
  }
  \item{x0}{
    Numeric or unit object indicating the starting x-location of the
    linear gradient.
  }
  \item{x1}{
    Numeric or unit object indicating the stopping x-location of the linear
    gradient.
  }
  \item{y0}{
    Numeric or unit object indicating the starting y-location of the linear
    gradient.
  }
  \item{y1}{
    Numeric or unit object indicating the stopping y-location of the linear
    gradient.
  }
  \item{x}{
    Numeric or unit object indicating the x-location of the radial
    gradient.
  }
  \item{y}{
    Numeric or unit object indicating the y-location of the radial
    gradient.
  }
  \item{r}{
    A numeric vector or unit object specifying the radius of the radial
    gradient.
  }
  \item{fx}{
    A numeric vector or unit object specifying an x-location. Determines
    the x-location of the focal point of the radial gradient.
  }
  \item{fy}{
    A numeric vector or unit object specifying an y-location. Determines
    the y-location of the focal point of the radial gradient.
  }
  \item{default.units}{
    A string indicating the default units to use if \code{x}, \code{y},
    \code{r}, \code{fx} or \code{fy} are only given as numeric vectors.
  }
  \item{spreadMethod}{
    A character vector determining when happens when a gradient begins
    or ends within its bounds. See details.
  }
}
\details{
  When defining gradient stops via \code{col} and \code{stops}, the
  order is important. Gradient stops which are defined earlier are drawn
  first, with later stops being drawn over the top.

  For \code{spreadMethod} the possible values are:
  \itemize{
    \item{pad}{
      Use the terminal colors of the gradient to fill the remainder of
      the target region.
    }
    \item{reflect}{
      Reflect the gradient pattern start-to-end, end-to-start,
      start-to-end, etc. continuously until the target region is filled.
    }
    \item{repeat}{
      Repeat the gradient pattern start-to-end, start-to-end,
      start-to-end, etc. continuously until the target region is filled.
    }
  }
}
\value{
   A \code{gradient} object.
}
\author{
  Simon Potter
}
