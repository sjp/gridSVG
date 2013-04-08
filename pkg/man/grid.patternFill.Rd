\name{grid.patternFill}
\alias{grid.patternFill}
\alias{patternFillGrob}
\title{ Associate a pattern fill with a grid grob }
\description{
  Creates a patternFilled.grob object which is a normal grid grob, with
  a pattern fill used in place of a regular fill. Used in conjunction
  with \code{patternFill}, to produce an SVG document containing
  graphical elements with pattern fills.
}
\usage{
grid.patternFill(path, label = NULL, pattern = NULL,
                 alpha = 1, group = TRUE, grep = FALSE)
patternFillGrob(x, label = NULL, pattern = NULL,
                alpha = 1, group = TRUE)
}
\arguments{
  \item{x}{
    A grob to add a pattern fill to.
  }
  \item{label}{
    A label that is associated with a definition of a pattern fill. This
    is the label used to create a pattern fill definition with
    \code{\link{patternFill}}.
  }
  \item{pattern}{
    A \code{pattern} object, provided by the \code{createPattern}
    function. Provides the definition of a pattern fill that will be
    applied to \code{x} or \code{path}.
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
  \item{grep}{
    Whether the \code{path} should be treated as a regular expression.
  }
}
\details{
  If \code{label} is specified, uses a pattern that has been supplied to
  \code{\link{patternFill}}. If \code{pattern} is specified it will be
  used as the fill pattern applied to each grob. If both are specified,
  it will attempt to define the pattern with the given label, as well
  as applying a pattern fill to the appropriate grobs.
}
\value{
  A patternFilled.grob object (for \code{patternFillGrob}).
}
\author{ Simon Potter }
\seealso{ \code{\link{patternFill}} }
