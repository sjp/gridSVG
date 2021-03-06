\name{Pattern Fills}
\alias{pattern}
\alias{registerPatternFill}
\alias{registerPatternFillRef}
\title{
  Create a definition of a fill pattern.
}
\description{
  A feature of SVG is that elements can be filled with a pattern that is
  defined somewhere in the document. The purpose of these functions is to
  create the definition of a fill pattern so that it can be referred to
  by grobs drawn by gridSVG.
}
\usage{
pattern(grob,
        x = unit(0, "npc"), y = unit(0, "npc"),
        width = unit(0.1, "npc"), height = unit(0.1, "npc"),
        default.units = "npc",
        just = "centre", hjust = NULL, vjust = NULL,
        dev.width = 7, dev.height = 7)
registerPatternFill(label, pattern = NULL, ...)
registerPatternFillRef(label, refLabel, pattern = NULL, ...)
}
\arguments{
  \item{label}{
    A character identifier for the definition.
  }
  \item{refLabel}{
    A character identifier referring to an existing pattern definition
    that has been created by \code{registerPatternFill}.
  }
  \item{pattern}{
    A \code{pattern} object created by \code{pattern}.
  }
  \item{grob}{
    A grid grob or tree of grobs.
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
  \item{dev.width, dev.height}{
    The width and height of the fill pattern's graphics region in
    inches. The default values are \code{7}.
  }
  \item{\dots}{
    Arguments to be be passed onto \code{pattern}.
  }
}
\details{
  The pattern fill is drawn off-screen on a new device. The size of this
  device is determined by \code{dev.width} and \code{dev.height}. The
  \code{grob} and \code{vp} that have been given are then drawn within
  this device. This is relevant for determining what the pattern
  definition looks like.

  The previous arguments do not determine the size of the pattern as it
  is being used (i.e. how big each "tile" is). This is set by the
  \code{x}, \code{y}, \code{width}, \code{height} arguments. The values
  of these arguments are relative to the current viewport as this
  function is being called. From then on, the definition of the location
  and size of the pattern are fixed.

  In summary, the \code{pattern} function defines what a pattern
  looks like, along with how big each tile is (and its position).

  To avoid repetition of pattern definitions, use
  \code{registerPatternFillRef} to reuse an existing pattern definition
  (referred to by \code{refLabel}). This means that a pattern "tile" can
  now be reused, repositioned and rescaled without having to describe
  how it needs to be drawn.

  In general use, first create a pattern object, then either give a
  label to the definition (for grobs to use), or alternatively simply
  pass on the pattern object to \code{\link{grid.patternFill}}.
}
\value{
  A \code{pattern} object for \code{pattern}, none otherwise.
}
\author{
  Simon Potter
}
\seealso{
\code{\link{grid.patternFill}}
}
