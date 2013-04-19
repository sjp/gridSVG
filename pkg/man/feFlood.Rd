\name{feFlood}
\alias{feFlood}
\title{
  Create and fill a rectangular region.
}
\description{
  This filter primitive creates a rectangle filled with a specified
  colour. The rectangle is as large as the filter primitive subregion
  established by the \code{x}, \code{y}, \code{width} and \code{height}
  attributes passed onto \code{\link{fe}} via \code{...}.
}
\usage{
feFlood(col = "black", ...)
}
\arguments{
  \item{col}{
    A colour that will be used to fill the filter region.
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
  An \code{fe.flood} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feFloodElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
