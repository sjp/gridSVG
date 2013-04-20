\name{grid.mask}
\alias{grid.mask}
\alias{maskGrob}
\title{
  Apply an opacity mask to a grid grob.
}
\description{
  Creates a \code{masked.grob} object which is a normal grid grob, with
  an opacity mask applied to it. Used in conjunction with
  \code{registerMask}, to produce an SVG document containing graphical
  elements with masked content.
}
\usage{
grid.mask(path, mask = NULL, label = NULL, group = TRUE, redraw = FALSE,
          strict = FALSE, grep = FALSE, global = FALSE)
maskGrob(x, mask = NULL, label = NULL, group = TRUE)
}
\arguments{
  \item{x}{
    A grob to mask.
  }
  \item{path}{
    A grob path specifying a drawn grob.
  }
  \item{mask}{
    A \code{mask} object, provided by the \code{\link{mask}} function.
    Provides the definition of an opacity mask that will be applied to
    \code{x} or \code{path}.
  }
  \item{label}{
    A label that is associated with a definition of an opacity mask.
    This is the label used to create an opacity mask definition with
    \code{registerMask}.
  }
  \item{group}{
    A logical vector that indicates whether the opacity mask should be
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
  If \code{label} is specified, uses a mask that has been supplied to
  \code{registerMask}. If \code{mask} is specified it will be used as
  the opacity mask applied to each grob. If both are specified, it will
  attempt to define the opacity mask with the given label, as well as
  applying the mask to the appropriate grobs.
}
\value{
  A \code{masked.grob} object (for \code{maskGrob}).
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{registerMask}}, \code{\link{mask}},
  \code{\link{pushMask}}.
}
