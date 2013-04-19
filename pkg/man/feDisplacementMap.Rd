\name{feDisplacementMap}
\alias{feDisplacementMap}
\title{
  Displace pixel values from a filter input.
}
\description{
  This filter primitive uses the pixels values from the image from
  \code{input2} to spatially displace the image from \code{input1}.
}
\usage{
feDisplacementMap(input1 = NA, input2 = NA,
                  scale = 0,
                  xChannelSelector = c("A", "R", "G", "B"),
                  yChannelSelector = c("A", "R", "G", "B"),
                  ...)
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
  \item{scale}{
    Displacement scale factor. The amount is expressed in the coordinate
    system established by attribute \code{primitiveUnits} on the parent
    filter container. When the value of \code{scale} is 0, this
    operation has no effect on the source image.
  }
  \item{xChannelSelector}{
    Indicates which channel from \code{input2} to use to displace the
    pixels in \code{input} along the x-axis.
  }
  \item{yChannelSelector}{
    Indicates which channel from \code{input2} to use to displace the
    pixels in \code{input} along the y-axis.
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
  An \code{fe.displacement.map} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feDisplacementMapElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
