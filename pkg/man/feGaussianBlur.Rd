\name{feGaussianBlur}
\alias{feGaussianBlur}
\title{
  Apply a Gaussian blur to an image.
}
\description{
  This filter effect primitive performs a Gaussian blur on the input
  image.
}
\usage{
feGaussianBlur(input = NA, sd = 0, ...)
}
\arguments{
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{sd}{
    The value of \code{sd} can be a numeric vector with either one or
    two elements. If two numbers are provided, the first number
    represents a standard deviation value along the x-axis of the
    current coordinate system and the second value represents a standard
    deviation in Y. If one number is provided, then that value is used
    for both X and Y.
  }
  \item{\dots}{
    Further arguments to be passed onto \code{\link{fe}}.
  }
}
\details{
  For more information about this primitive, consult the reference to
  the SVG specification.
}
\value{
  An \code{fe.gaussian.blur} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feGaussianBlurElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
