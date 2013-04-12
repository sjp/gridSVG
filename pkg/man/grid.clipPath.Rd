\name{grid.clipPath}
\alias{grid.clipPath}
\alias{clipPathGrob}
\title{
  Apply a custom clipping path.
}
\description{
  A feature of SVG is that elements can be clipped to by more than just
  a rectangular region. Most graphical elements can be drawn. The
  purpose of these functions is to define a more sophisticated clipping
  path that will be applied until the current viewport is popped.
}
\usage{
grid.clipPath(...)
clipPathGrob(grob, name = NULL)
}
\arguments{
  \item{grob}{
    A grob that defines a clipping path region.
  }
  \item{name}{
    A character identifier.
  }
  \item{\dots}{
    Arguments to be passed onto \code{clipPathGrob}.
  }
}
\details{

  A clipping path will be drawn within the current viewport. The effect
  is that as soon as the clipping grob has been drawn, its boundaries
  are now the clipping region, until the current viewport is popped.

  Most grobs can be used for clipping but there are some limitations on
  what will actually be used for clipping. In general though, anything
  that is drawn as the clipping path will have the union of its drawn
  regions become the new region that the current viewport will clip to.

  The limitations are as follows:

  \itemize{
    \item Any viewport pushed by the clipping path grob will no longer clip to its
    contents. However, its clipping region will remain. This means that
    the clipping region for a pushed viewport will become the union of its
    contents and the viewport clipping region itself, instead of just
    the pushed viewport's clipping region.

    \item No \code{pointGrob}s are able to be used for clipping.

    \item Any operations that apply to containers (e.g. gpars,
    garnishing, animation), will no longer work. Any operations that are
    not applied to groups are unaffected. This affects in particular
    viewports, gTrees, and the familiar gridSVG grob grouping that
    occurs.
  }
}
\value{
  \code{clipPathGrob} returns a \code{clipPath} object.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.mask}}, \code{\link{grid.clip}}
}
