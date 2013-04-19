\name{feTile}
\alias{feTile}
\title{
  Fill a rectangle with a tiled pattern of an input image.
}
\description{
  This filter primitive fills a target rectangle with a repeated, tiled
  pattern of an input image. The target rectangle is as large as the
  filter primitive subregion established by the \code{x}, \code{y},
  \code{width} and \code{height} arguments that are passed onto
  \code{\link{fe}} by \code{\link{feTile}}.
}
\usage{
feTile(input = NA, ...)
}
\arguments{
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
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
  An \code{fe.tile} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feTileElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
