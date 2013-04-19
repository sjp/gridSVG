\name{feComposite}
\alias{feComposite}
\title{
  Combine images using Porter-Duff operations.
}
\description{
  This filter performs the combination of the two input images
  pixel-wise in image space using one of the Porter-Duff
  compositing operations.

  The arithmetic operation is useful for combining the output from the
  \code{\link{feDiffuseLighting}} and \code{\link{feSpecularLighting}}
  filter effects with texture data. It is also useful for implementing
  dissolve.
}
\usage{
feComposite(input1 = NA, input2 = NA,
            operator = c("over", "in", "out", "atop",
                         "xor", "arithmetic"),
            k1 = 0, k2 = 0, k3 = 0, k4 = 0, ...)
}
\arguments{
  \item{input1}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{input2}{
    Identfies a second input for this filter primitive. See
    \code{\link{filterInputs}}.
  }
  \item{operator}{
    The compositing operation that is to be performed. All of the
    \code{operator} types except \code{"arithmetic"} match the
    corresponding operation as described in the referenced Porter-Duff
    text. The arithmetic operator is described in the referenced SVG
    specification.
  }
  \item{k1}{
    A numeric value. Only applicable if \code{operator} is
    \code{"arithmetic"}.
  }
  \item{k2}{
    A numeric value. Only applicable if \code{operator} is
    \code{"arithmetic"}.
  }
  \item{k3}{
    A numeric value. Only applicable if \code{operator} is
    \code{"arithmetic"}.
  }
  \item{k4}{
    A numeric value. Only applicable if \code{operator} is
    \code{"arithmetic"}.
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
  An \code{fe.composite} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feCompositeElement}

  Compositing Digital Images, T. Porter and T. Duff. SIGGRAPH '84
  Conference Proceedings, Association for Computing Machinery, Volume
  18, Number 3, July 1984.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
