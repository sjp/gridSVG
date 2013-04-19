\name{fe}
\alias{fe}
\title{
  Creating a generic filter effect
}
\description{
  This function creates an object that contains all of the basic
  attributes that each filter effect inherits from. This is not intended
  to be used directly, instead it is to be used as a convenience
  function for building up filter effect objects.
}
\usage{
fe(...,
   x = unit(0.5, "npc"), y = unit(0.5, "npc"),
   width = unit(1, "npc"), height = unit(1, "npc"),
   just = "centre", hjust = NULL, vjust = NULL,
   default.units = "npc", result = NULL)
}
\arguments{
  \item{\dots}{
    Further attributes to add to the object.
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
  \item{result}{
    A character identifier, naming the result of the filter operation.
    The result can be used an an input to some filter effects.
  }
}
\value{
  A \code{filter.effect} object.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}
}
