\name{Gradient Objects}
\alias{linearGradient}
\alias{radialGradient}
\title{
  Create Linear and Radial Gradients
}
\description{
  Create objects which describe linear and radial gradients. These
  objects can then be used to add gradient stops. These objects can
  later be used to apply a gradient fill to grobs, and also to define a
  gradient so that it may be reused multiple times.
}
\usage{
linearGradient(gradientUnits = c("bbox", "coords"),
               x = unit(c(0, 1), "npc"), y = unit(c(0, 1), "npc"),
               default.units = "npc",
               spreadMethod = c("pad", "reflect", "repeat"),
               stops = NULL)
radialGradient(gradientUnits = c("bbox", "coords"),
               x = unit(0.5, "npc"), y = unit(0.5, "npc"),
               r = unit(0.5, "npc"),
               fx = unit(0.5, "npc"), fy = unit(0.5, "npc"),
               default.units = "npc",
               spreadMethod = c("pad", "reflect", "repeat"),
               stops = NULL)
}
\arguments{
  \item{gradientUnits}{
    If \code{"bbox"}, the gradient is positioned relative to the
    bounding box of the referring grob.

    If \code{"coords"}, uses grid coordinates to determine positioning.
  }
  \item{x}{
    A numeric vector or unit object specifying x-location(s). For linear
    gradients this must have elements, corresponding to the start and
    end x-location.
  }
  \item{y}{
    A numeric vector or unit object specifying y-location(s). For linear
    gradients this must have elements, corresponding to the start and
    end y-location.
  }
  \item{r}{
    A numeric vector or unit object specifying a radius.
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
  \item{stops}{
    If not \code{NULL}, a list of \code{gradient.stop} objects. See
    \code{\link{gradientStop}}.
  }
}
\details{
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
