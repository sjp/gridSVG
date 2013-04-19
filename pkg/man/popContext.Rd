\name{popContext}
\alias{popContext}
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
popContext(n = 1)
}
\arguments{
  \item{n}{
    The number of contexts to pop. A warning will be given when \code{n}
    is greater than the number that has been applied.
  }
}
\details{
  Popping a context can produce a warning. In this case it is
  recommended that the context "pushing" and "popping" be revised to
  have matching pairs of pushes and pops.
}
\value{
  None.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.clipPath}} and \code{\link{grid.mask}}
}
