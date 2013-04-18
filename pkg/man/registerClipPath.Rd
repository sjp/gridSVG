\name{Clipping Paths}
\alias{registerClipPath}
\title{
  Create the definition of a non-rectangular clipping path.
}
\description{
  A feature of SVG is that elements can be clipped to by more than just
  a rectangular region. Most graphical elements can be drawn. The
  purpose of these functions is to define a more sophisticated clipping
  path that will be applied until the current viewport (or context, see
  \code{\link{grid.popContext}}) is popped.
}
\usage{
registerClipPath(label, clippath)
}
\arguments{
  \item{label}{
    A character identifier that will be used to reference this
    definition.
  }
  \item{clippath}{
    A grob that defines a clipping path region.
  }
}
\details{
  A clipping path will be drawn within the current viewport at the time
  of definition (if the grob has no \code{vp} specified).

  Most grobs can be used for clipping but there are some limitations on
  what will actually be used for clipping. In general though, anything
  that is drawn as the clipping path will have the union of its drawn
  regions become the new region that the current viewport (or grob) will
  clip to.

  The limitations are as follows:

  \itemize{

    \item Any viewport pushed by the clipping path grob will no longer
    clip to its contents. However, its clipping region will remain. This
    means that the clipping region for a pushed viewport will become the
    union of its contents and the viewport clipping region itself,
    instead of just the pushed viewport's clipping region.

    \item When drawing a \code{textGrob}, only character labels will be
    used, no \code{plotmath} expressions will be used.

    \item No \code{pointGrob}s are able to be used for clipping.

    \item Any operations that apply to containers (e.g. gpars,
    garnishing, animation), will no longer work. Any operations that are
    not applied to groups are unaffected. This affects in particular
    viewports, gTrees, and the familiar gridSVG grob grouping that
    occurs.

  }
}
\value{
  None
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.popContext}}, \code{\link{grid.clipPath}},
  \code{\link{pushClipPath}}, \code{\link{grid.clip}}
}
