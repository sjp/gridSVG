\name{grid.clipPath}
\alias{grid.clipPath}
\alias{clipPathGrob}
\title{
  Apply a clipping path to a grid grob.
}
\description{
  Creates a \code{pathClipped.grob} object which is a normal grid grob,
  with a clipping path applied to it. Used in conjunction with
  \code{registerClipPath}, to produce an SVG document containing
  graphical elements with masked content.
}
\usage{
grid.clipPath(path, clippath = NULL, label = NULL, group = TRUE, grep = FALSE)
clipPathGrob(x, clippath = NULL, label = NULL, group = TRUE)
}
\arguments{
  \item{x}{
    A grob to clip.
  }
  \item{path}{
    A grob path specifying a drawn grob.
  }
  \item{clippath}{
    A grob defining a clipping region.
  }
  \item{label}{
    A label that is associated with a definition of a clipping path.
    This is the label used to make a clipping path definition with
    \code{registerClipPath}.
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
  supplied to \code{registerClipPath}. If \code{clippath} is specified
  it will be used as the clipping path applied to each grob. If both are
  specified, it will attempt to define the clipping path with the given
  label, as well as applying the clipping path to the appropriate grobs.
}
\value{
  A \code{pathClipped.grob} object (for \code{clipPathGrob}).
}
\author{
  Simon Potter.
}
\seealso{
  \code{\link{registerClipPath}}, \code{\link{pushClipPath}}.
}
