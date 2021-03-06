\name{grid.patternFill}
\alias{grid.patternFill}
\alias{patternFillGrob}
\title{ Associate a pattern fill with a grid grob }
\description{
  Creates a patternFilled.grob object which is a normal grid grob, with
  a pattern fill used in place of a regular fill. Used in conjunction
  with \code{registerPatternFill}, to produce an SVG document containing
  graphical elements with pattern fills.
}
\usage{
grid.patternFill(path, pattern = NULL, label = NULL,
                 alpha = 1, group = TRUE, redraw = FALSE,
                 strict = FALSE, grep = FALSE, global = FALSE)
patternFillGrob(x, pattern = NULL, label = NULL,
                alpha = 1, group = TRUE)
}
\arguments{
  \item{x}{
    A grob to add a pattern fill to.
  }
  \item{pattern}{
    A \code{pattern} object, provided by the \code{pattern}
    function. Provides the definition of a pattern fill that will be
    applied to \code{x} or \code{path}.
  }
  \item{label}{
    A label that is associated with a definition of a pattern fill. This
    is the label used to create a pattern fill definition with
    \code{\link{registerPatternFill}}.
  }
  \item{path}{
    A grob path specifying a drawn grob.
  }
  \item{alpha}{
    The alpha channel for transparency. A value between 0 and 1.
  }
  \item{group}{
    A logical vector that indicates whether the pattern fill should be
    applied to the overall parent group for the relevant SVG element, or
    to individual SVG elements.
  }
  \item{redraw}{
    A logical value to indicate whether to redraw the grob.
  }
  \item{strict}{
    A boolean indicating whether the \code{path} must be matched
    exactly.
  }
  \item{grep}{
    Whether the \code{path} should be treated as a regular expression.
  }
  \item{global}{
    A boolean indicating whether the function should affect just the
    first match of the \code{path}, or whether all matches should be
    affected.
  }
}
\details{
  If \code{label} is specified, uses a pattern that has been supplied to
  \code{\link{registerPatternFill}}. If \code{pattern} is specified it
  will be used as the fill pattern applied to each grob. If both are
  specified, it will attempt to define the pattern with the given label,
  as well as applying a pattern fill to the appropriate grobs.
}
\value{
  A patternFilled.grob object (for \code{patternFillGrob}).
}
\author{ Simon Potter }
\seealso{ \code{\link{registerPatternFill}} }
