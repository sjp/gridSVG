\name{grid.comment}
\alias{grid.comment}
\alias{commentGrob}
\title{ Create a grid grob representing a comment }
\description{
  Creates a \code{comment.grob} object which is a grid
  \code{\link{nullGrob}}, with a comment attached. Useful in conjunction
  with \code{gridToSVG}, to produce an SVG document with comments
  inserted at the point where the grob is \dQuote{drawn}.
}
\synopsis{
grid.comment(comment, name = NULL, vp = NULL)
commentGrob(comment, name = NULL, vp = NULL)
}
\usage{
grid.comment(comment, name = NULL, vp = NULL)
}
\arguments{
  \item{comment}{
    A character vector used to write out a comment. If this has a length
    greater than one, each element is assumed to be a line.
  }
  \item{name}{ A character identifier. }
  \item{vp}{ The viewport to which the grob belongs. }
}
\value{
  A \code{comment.grob} object.
}
\author{ Simon Potter }
\seealso{ \code{\link{gridToSVG}} }
\keyword{ dplot }

