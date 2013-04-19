\name{feImage}
\alias{feImage}
\title{
  Draw a referred image.
}
\description{
  This filter effect primitive refers to a graphic external to this
  filter container, which is loaded or rendered into an RGBA raster and
  becomes the result of the filter effect primitive.
}
\usage{
feImage(preserveAspectRatio = "xMidYMid meet", href = "", ...)
}
\arguments{
  \item{preserveAspectRatio}{
    See references for appropriate values and behaviour.
  }
  \item{href}{
    A \code{URL} reference to a stand-alone image resource such as a
    JPEG, PNG or SVG file. e.g. \code{http://example.com/img.jpg}
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
  An \code{fe.image} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feImageElement}
  \url{http://www.w3.org/TR/SVG/coords.html#PreserveAspectRatioAttribute}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
