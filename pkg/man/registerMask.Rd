\name{Opacity Masks}
\alias{mask}
\alias{registerMask}
\title{
  Create the definition of an opacity mask.
}
\description{
  A feature of SVG is that elements can have an opacity mask applied to
  it. An opacity mask is an image that, for various levels of opacity,
  makes the object that is being masked inherit the same levels of
  opacity. The purpose of these functions is to define an opacity mask
  that will be applied until the current viewport (or context, see
  \code{\link{popContext}}) is popped. Alternatively it can also be
  applied to grobs.
}
\usage{
mask(grob,
     x = unit(0.5, "npc"), y = unit(0.5, "npc"),
     width = unit(1, "npc"), height = unit(1, "npc"),
     default.units = "npc",
     just = "centre", hjust = NULL, vjust = NULL)
registerMask(label, mask = NULL, ...)
}
\arguments{
  \item{grob}{
    A grob or gTree that will be drawn as the opacity mask.
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
  \item{label}{
    A character identifier that will be used to reference this
    definition.
  }
  \item{mask}{
    A \code{mask} object that defines the mask.
  }
  \item{\dots}{
    Arguments to be given to \code{\link{mask}}.
  }
}
\details{
  When registering the mask, the rectangular region that the mask
  applies to will become fixed.

  When referring to an opacity mask, the masked content will be opaque
  at the same coordinates that the mask is opaque. The same applies when
  there is any level of transparency, as any transparency in the mask
  will also apply in the same corresponding region of the masked object.

  The mask's opacity is defined as being the level of luminance present
  in the mask. This means anything black is fully transparent, while
  anything white is completely opaque. The background is assumed to be
  black (i.e. fully transparent). The \code{alpha} value in a mask will
  still be used, but its effect is combined with the computed opacity
  from the luminance of the mask.

  By using an opacity mask it is possible to have a grob with
  non-uniform opacity. In other words, rather than specifying an opacity
  via \code{\link{gpar}}'s \code{alpha} parameter, which is uniform
  across the grob, we can define varying opacities on a grob via an
  opacity mask.

  The \code{x}, \code{y}, \code{width}, \code{height} parameters
  determine the location and dimensions of the area to apply the mask
  to. This means we can apply a mask to any rectangular region, relative
  to the viewport in which it is defined (via \code{registerMask}).
}
\value{
  For \code{mask}, a \code{mask} object.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.mask}}, \code{\link{pushMask}},
  \code{\link{popContext}}.
}
