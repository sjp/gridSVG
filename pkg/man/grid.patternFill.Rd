\name{grid.patternFill}
\alias{grid.patternFill}
\alias{patternFillGrob}
\title{ Associate a pattern fill with a grid grob }
\description{
  Creates a patternFilled.grob object which is a normal grid grob, with
  a pattern fill used in place of a regular fill. Used in conjunction
  with \code{fillPattern}, to produce an SVG document containing
  graphical elements with pattern fills.
}
\usage{
grid.patternFill(path, label, alpha = 1, group = TRUE, grep = FALSE)
patternFillGrob(x, label, alpha = 1, group = TRUE)
}
\arguments{
  \item{x}{
    A grob to add a pattern fill to.
  }
  \item{label}{
    A label that is associated with a definition of a pattern fill. This
    is the label used to create a pattern fill definition with
    \code{\link{fillPattern}}.
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
\value{
  A patternFilled.grob object.
}
\author{ Simon Potter }
\seealso{ \code{\link{fillPattern}} }
