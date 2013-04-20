\name{grid.gradientFill}
\alias{grid.gradientFill}
\alias{gradientFillGrob}
\title{
  Associate a gradient fill with a grid grob
}
\description{
  Creates a \code{gradientFilled.grob} object which is a normal grid
  grob, with a gradient fill used in place of a regular fill. Used in
  conjunction with \code{registerGradientFill}, to produce an SVG
  document containing graphical elements with gradient fills.
}
\usage{
grid.gradientFill(path, gradient = NULL, label = NULL,
                  alpha = 1, group = TRUE, redraw = FALSE,
                  strict = FALSE, grep = FALSE, global = FALSE)
gradientFillGrob(x, gradient = NULL, label = NULL,
                 alpha = 1, group = TRUE)
}
\arguments{
  \item{x}{
    A grob to add a pattern fill to.
  }
  \item{path}{
    A grob path specifying a drawn grob.
  }
  \item{gradient}{
    A \code{gradient} object, provided by the \code{linearGradient} and
    \code{radialGradient} functions. Provides the definition of a
    gradient fill that will be applied to \code{x} or \code{path}.
  }
  \item{label}{
    A label that is associated with a definition of a gradient
    fill. This is the label used to create a gradient fill
    definition with \code{registerGradientFill}.
  }
  \item{alpha}{
    The alpha channel for transparency. A value between 0 and 1.
  }
  \item{group}{
    A logical vector that indicates whether the gradient fill
    should be applied to the overall parent group for the
    relevant SVG element, or to individual SVG elements.
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
  If \code{label} is specified, uses a gradient that has been supplied
  to \code{registerGradientFill}. If \code{gradient} is specified it
  will be used as the gradient fill applied to each grob. If both are
  specified, it will attempt to define the gradient with the given
  label, as well as applying a gradient fill to the appropriate grobs.
}
\value{
  A \code{gradientFilled.grob} object (for \code{gradientFillGrob}).
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{linearGradient}}, \code{\link{radialGradient}},
  \code{\link{registerGradientFill}}
}
