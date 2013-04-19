\name{feMerge}
\alias{feMerge}
\alias{feMergeNode}
\alias{addMergeNode}
\title{
  Composite image layers together.
}
\description{
  This filter primitive composites input image layers on top of each
  other using the "over" operator with "input1" (corresponding to the
  first child merge node) on the bottom and the last specified input,
  "inputN" (corresponding to the last child merge node), on top.
}
\usage{
feMerge(mergeNodes = NULL, ...)
addMergeNode(fe, mergeNode, after = NA)
feMergeNode(input = NA)
}
\arguments{
  \item{mergeNodes}{
    A list of \code{fe.merge.node} objects, produced by
    \code{\link{feMergeNode}}.
  }
  \item{\dots}{
    Further arguments to be passed onto \code{\link{fe}}.
  }
  \item{fe}{
    An \code{fe.merge} object, created by \code{\link{feMerge}}.
  }
  \item{mergeNode}{
    An \code{fe.merge.node} object, created by \code{\link{feMerge}}.
  }
  \item{after}{
    The position to add \code{mergeNode} to in the list of \code{fe}'s
    children. When \code{NA}, appends to the end of the list of
    children.
  }
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
}
\details{
  If you wish to add more merge nodes after an \code{fe.merge} object
  has been created, use \code{\link{addMergeNode}} to add merge nodes to
  the filter primitive.

  For more information about the \code{feMerge} primitive, consult the
  reference to the SVG specification.
}
\value{
  For \code{feMerge}, an \code{fe.merge} object.

  For \code{addMergeNode}, an \code{fe.merge} object.

  For \code{feMergeNode}, an \code{fe.merge.node} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feMergeElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
