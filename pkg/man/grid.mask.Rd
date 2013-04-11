\name{grid.mask}
\alias{grid.mask}
\alias{maskGrob}
\title{
  Apply an opacity mask to a grob.
}
\description{
  Creates a \code{masked.grob} object which is a normal grid grob, with
  a defined opacity mask applied. Used in conjunction with
  \code{\link{createMask}}, to produce an SVG document containing
  graphical elements with opacity masks applied.
}
\usage{
grid.mask(path, label = NULL, mask = NULL,
          group = TRUE, grep = FALSE)
maskGrob(x, label = NULL, mask = NULL, group = TRUE)
}
\arguments{
  \item{x}{
    A grob to add an opacity mask to.
  }
  \item{path}{
    A grob path specifyng a drawn grob.
  }
  \item{label}{
    A label that is associated with a definition of an opacity
    mask. This is the label used to create an mask definition with
    \code{defineMask}.
  }
  \item{mask}{
    A \code{mask} object, provided by the \code{\link{createMask}}
    function. Provides the definition of an opacity mask that will be
    applied to \code{x} or \code{path}.
  }
  \item{group}{
    A logical vector that indicates whether the opacity mask should be
    applied to the overall parent group for the relevant SVG element, or
    to individual SVG elements.
  }
  \item{grep}{
    Whether the \code{path} should be treated as a regular expression.
  }
}
\details{
  If \code{label} is specified, uses a clipping path that has been
  supplied to \code{clipPath}. If \code{clippath} is specified it will
  be used as the clipping path applied to each grob. If both are
  specified, it will attempt to define the given clipping path with the
  given label, as well as applying the clipping path to the appropriate
  grobs.

  When referring to an opacity mask, the masked grob will be opaque at
  the same coordinates that the mask is opaque. The same applies when
  there is any level of transparency, as any transparency in the mask
  will also apply in the same corresponding region of the masked object.

  By using an opacity mask it is possible to have a grob with
  non-uniform opacity. In other words, rather than specifying an opacity
  via \code{\link{gpar}}'s \code{alpha} parameter, which is uniform
  across the grob, we can define varying opacities on a grob via an
  opacity mask.
}
\value{
  A \code{masked.grob} object (for \code{maskGrob}).
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{createMask}}
}
