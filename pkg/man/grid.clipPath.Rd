\name{grid.clipPath}
\alias{grid.clipPath}
\alias{clipPathGrob}
\title{
  Apply a custom clipping path to a grob.
}
\description{
  Creates a \code{pathClipped.grob} object which is a normal grid grob,
  with a defined clipping path applied. Used in conjunction with
  \code{\link{clipPath}}, to produce an SVG document containing
  graphical elements with arbitrary clipping paths.
}
\usage{
grid.clipPath(path, label = NULL,
              clippath = NULL, cliprule = "winding",
              group = TRUE, grep = FALSE)
clipPathGrob(x, label = NULL,
             clippath = NULL, cliprule = "winding",
             group = TRUE)
}
\arguments{
  \item{x}{
    A grob to add a clipping path to.
  }
  \item{path}{
    A grob path specifying a drawn grob.
  }
  \item{label}{
    A label that is associated with a definition of a clipping
    path. This is the label used to create a clipping path definition
    with \code{clipPath}.
  }
  \item{clippath}{
    A \code{clipping.path} object, provided by the
    \code{\link{createClippingPath}} function. Provides the definition
    of a clipping path that will be applied to \code{x} or \code{path}.
  }
  \item{cliprule}{
    A character value specifying the fill rule: either
    \code{"winding"} or \code{"evenodd"}. See \code{\link{grid.path}}
    (particularly its examples) for more details.
  }
  \item{group}{
    A logical vector that indicates whether the clipping path should be
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
}
\value{
  A \code{pathClipped.grob} object (for \code{clipPathGrob}).
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{clipPath}}
}
