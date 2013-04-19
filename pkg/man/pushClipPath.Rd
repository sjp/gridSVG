\name{pushClipPath}
\alias{pushClipPath}
\alias{popClipPath}
\title{
  Apply a clipping context to the current viewport.
}
\description{
  This function is intended to be used similarly to
  \code{\link{grid.clip}}. The only difference is that a non-rectangular
  clipping region can be applied.
}
\usage{
pushClipPath(clippath = NULL, label = NULL, name = NULL, draw = TRUE)
popClipPath()
}
\arguments{
  \item{clippath}{
    A graphics object, used as the definition of a clipping path.
  }
  \item{label}{
    A label for a defined reference.
  }
  \item{name}{
    A character identifier for the grob applying the clipping context.
  }
  \item{draw}{
    A logical value indicating whether graphics output should be
    produced.
  }
}
\details{
  If \code{label} is specified, uses a clipping path that has been
  supplied to \code{registerClipPath}. If \code{clippath} is specified
  it will be used as the new clipping context for the current
  viewport. If both are specified, it will attempt to define the
  clipping path with the given label, as well as adding the clipping
  path as a clipping context for the current viewport.

  \code{popClipPath} is an alias for \code{\link{popContext}}
}
\value{
  A \code{pushClipPath} grob. The value is returned invisibly.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{registerClipPath}}, \code{\link{grid.clipPath}},
  \code{\link{popContext}}.
}
