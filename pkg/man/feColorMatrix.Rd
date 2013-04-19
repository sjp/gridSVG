\name{feColorMatrix}
\alias{feColorMatrix}
\title{
  Apply a matrix transformation on colour values.
}
\description{
  This filter applies a matrix transformation on the RGBA colour and
  alpha values of every pixel on the input graphics to produce a result
  with a new set of RGBA colour and alpha values.
}
\usage{
feColorMatrix(input = NA,
              type = c("matrix", "saturate",
                       "hueRotate", "luminanceToAlpha"),
              values = NULL, ...)
}
\arguments{
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{type}{
    Indicates the type of matrix operation. The keyword \code{"matrix"}
    indicates that a full 5x4 matrix of values will be provided. The
    other keywords represent convenience shortcuts to allow commonly
    used color operations to be performed without specifying a complete
    matrix.
  }
  \item{values}{
    The contents of \code{values} depend on what \code{type} is:

    \itemize{
      \item \code{matrix} A 5x4 matrix of numeric values.

      \item \code{saturate} A single element numeric vector whose value
      is between 0 and 1.

      \item \code{hueRotate} A single element numeric vector whose value
      represents degrees.

      \item \code{luminanceToAlpha} Should be left as \code{NULL} as
      there are no applicable values.
    }
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
  An \code{fe.color.matrix} object.
}
\references{
  \url{www.w3.org/TR/SVG/filters.html#feColorMatrixElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
