\name{filterEffect}
\alias{filterEffect}
\alias{addFilterEffect}
\title{
  Creating Filter Effects
}
\description{
  Create objects which describe filter effects. These objects can be
  used to add filter effect primitives. They can be used to apply a
  filter effect to grobs and also to define a filter effect so that it
  may be used multiple times.
}
\usage{
filterEffect(filterUnits = c("coords", "bbox"),
             x = unit(0.5, "npc"), y = unit(0.5, "npc"),
             width = unit(1, "npc"), height = unit(1, "npc"),
             just = "centre", hjust = NULL, vjust = NULL,
             default.units = "npc",
             primitiveUnits = c("coords", "bbox"),
             filterEffects = NULL)
addFilterEffect(filter, filterEffect, after = NA)
}
\arguments{
  \item{filterUnits}{
    If \code{"bbox"}, the filter effect itself is positioned relative to
    the bounding box of the referring grob. All units attempt to be
    converted to equivalent "npc" coordinates as a reulst.

    If \code{"coords"}, uses grid coordinates to determine positioning.
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
  \item{default.units}{
    A string indicating the default units to use if \code{x}, \code{y},
    \code{width}, or \code{height} are only given as numeric vectors.
  }
  \item{primitiveUnits}{
    If \code{"bbox"}, all filter effect primitives will be positioned
    relative to the bounding box of the filter effect region (determined
    by \code{x}, \code{y}, \code{width}, \code{height} and
    \code{filterUnits}). All units attempt to be converted to equivalent
    "npc" coordinates.

    If \code{"coords"}, uses grid coordinates to determine positioning.
  }
  \item{filterEffects}{
    A list of filter effect primitives. For example a list containing a
    gaussian blur primitive created by \code{\link{feGaussianBlur}}.
  }
  \item{filter}{
    A filter effect container object, as created by \code{filterEffect}.
  }
  \item{filterEffect}{
    A filter effect primitive object.
  }
  \item{after}{
    Numeric. Determines where amongst the children of \code{filter} that
    \code{filterEffect} should be added. \code{NA} indicates that
    \code{filterEffect} should be appended to the end of the list of
    children.
  }
}
\details{
  This is primarily a container object to hold filter effect primitives.
}
\value{
  A \code{filter} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#FilterElement}
}
\author{
  Simon Potter
}
\seealso{
  Any of the filter effect primitives (named \code{fe*}),
  e.g. \code{\link{feGaussianBlur}}.
}
