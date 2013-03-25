\name{grid.gaussianBlur}
\alias{grid.gaussianBlur}
\alias{gaussianBlurGrob}
\title{
  Apply a Gaussian Blur to a Grob.
}
\description{
  Creates a gaussianBlurred.grob object which is a normal grid grob,
  with a Gaussian blur applied to it. Used in conjunction with
  \code{\link{gaussianBlur}}, to produce an SVG document containing
  graphical elements with Gaussian blurs applied.
}
\usage{
grid.gaussianBlur(path, label, group = TRUE, grep = FALSE)
gaussianBlurGrob(x, label, group = TRUE)
}
\arguments{
  \item{x}{
    A grob.
  }
  \item{path}{
    A grob path specifying a drawn grob.
  }
  \item{label}{
    A label that is associated with a definition of a Gaussian blur.
    This is the label used when creating a Gaussian blur with
    \code{\link{gaussianBlur}}.
  }
  \item{group}{
    A logical vector that indicates whether the Gaussian blur should be
    applied to the overall parent group for the relevant SVG element, or
    to individual SVG elements.
  }
  \item{grep}{
    Whether the \code{path} should be treated as a regular expression.
  }
}
\value{
  A \code{gaussianBlurred.grob} object.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{gaussianBlur}}
}
