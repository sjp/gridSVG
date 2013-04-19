\name{pushMask}
\alias{pushMask}
\alias{popMask}
\title{
  Apply a masking context to the current viewport.
}
\description{
  This function is intended to be used similarly to
  \code{\link{grid.clip}}. The key difference is that instead of
  applying a new clipping context to the viewport, we apply a new
  masking context.
}
\usage{
pushMask(mask = NULL, label = NULL, name = NULL, draw = TRUE)
popMask()
}
\arguments{
  \item{mask}{
    A \code{mask} object, used as the definition of an opacity mask.
  }
  \item{label}{
    A label for a defined reference.
  }
  \item{name}{
    A character identifier for the grob applying the masking context.
  }
  \item{draw}{
    A logical value indicating whether graphics output should be
    produced.
  }
}
\details{
  If \code{label} is specified, uses a mask that has been supplied to
  \code{registerMask}. If \code{mask} is specified it will be used as
  the new masking context for the current viewport. If both are
  specified, it will attempt to define the mask with the given label, as
  well as applying the mask as the new masking context for the current
  viewport.

  \code{popMask} is an alias for \code{\link{grid.popContext}}.
}
\value{
  A \code{pushMask} grob. The value is returned invisibly.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{mask}}, \code{\link{registerMask}},
  \code{\link{grid.mask}}, \code{\link{grid.popContext}}.
}
