\name{createMask}
\alias{defineMask}
\alias{createMask}
\title{
  Create the definition of an opacity mask.
}
\description{
  A feature of SVG is that elements can have an opacity mask applied to
  it. An opacity mask is an image drawn off-screen that, when opaque,
  makes the object that is being masked opaque. The purpose of these
  functions is to create the definition of an opacity mask so that it
  can be referred to by grobs drawn by gridSVG.
}
\usage{
createMask(grob = NULL, vp = NULL,
           x = unit(0.5, "npc"), y = unit(0.5, "npc"),
           width = unit(1, "npc"), height = unit(1, "npc"),
           default.units = "npc",
           just = "centre", hjust = NULL, vjust = NULL)
defineMask(label, mask = NULL, ...)
}
\arguments{
  \item{label}{
    A character identifier for the definition.
  }
  \item{mask}{
    A \code{"mask"} object, created by the \code{createMask} function.
  }
  \item{\dots}{
    Arguments to be passed onto \code{createMask}.
  }
  \item{grob}{
    A grid grob or tree of grobs.
  }
  \item{vp}{
    A grid viewport object. If not \code{NULL}, this will be pushed into
    prior to drawing the \code{grob}.
  }
  \item{x}{
    A numeric vector or unit object specifying x-location.
  }
  \item{y}{
    A numeric vector or unit object specifying y-location.
  }
  \item{width}{
    A numeric vector or unit object specifying width.
  }
  \item{height}{
    A numeric vector or unit object specifying height.
  }
  \item{default.units}{
    A string indicating the default units to use if \code{x}, \code{y},
    \code{width}, or \code{height} are only given as numeric vectors.
  }
  \item{just}{
    The justification of the pattern relative to its (x, y) location.
    If there are two values, the first value specifies horizontal
    justification and the second value specifies vertical justification.
    Possible string values are: \code{"left"}, \code{"right"},
    \code{"centre"}, \code{"center"}, \code{"bottom"}, and \code{"top"}.
    For numeric values, 0 means left alignment and 1 means right
    alignment.
  }
  \item{hjust}{
    A numeric vector specifying horizontal justification. If specified,
    overrides the \code{just} setting.
  }
  \item{vjust}{
    A numeric vector specifying vertical justification. If specified,
    overrides the \code{just} setting.
  }
}
\details{
  The opacity mask is drawn off-screen on a new device. This device has
  the same dimensions as the current graphics device. The \code{grob}
  and \code{vp} that have been given are then drawn on this new device.

  The \code{x}, \code{y}, \code{width}, \code{height} parameters
  determine the location and dimensions of the area to apply the mask
  to. These locations and dimensions become fixed at the time of
  definition.
}
\value{
  For \code{createMask}, a \code{mask} object. None for
  \code{defineMask}.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.mask}}
}
