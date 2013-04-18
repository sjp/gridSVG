\name{grid.popContext}
\alias{grid.popContext}
\alias{popContextGrob}
\title{
  Leaving A Modified Viewport Context
}
\description{
  A modified viewport context is where the appearance of grobs is no
  longer determined solely by the grob itself and the viewport into
  which they're drawn. This can occur when applying clipping paths and
  opacity masks, which modify the appearance of anything drawn after
  they have been applied. This function should be used when attempting
  to stop the effect of a modified viewport context (e.g. to stop
  clipping to paths).
}
\usage{
grid.popContext(...)
popContextGrob(n = 1)
}
\arguments{
  \item{n}{
    The number of contexts to pop. A warning will be given when \code{n}
    is greater than the number that has been applied.
  }
  \item{\dots}{
    Arguments to be passed onto \code{popContextGrob}.
  }
}
\details{
  Both functions create a \code{popContext} grob, but
  \code{grid.popContext} draws the grob immediately.

  Popping a context may sometimes be ignored. This will happen when a
  "pop" does not occur in the same location in the tree that a context
  was "pushed". For example this happens when a \code{gTree} is drawn
  that has a child \code{popContextGrob}, but no child context being
  pushed.
}
\value{
  A \code{popContext} grob.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.clipPath}} and \code{\link{grid.mask}}
}
