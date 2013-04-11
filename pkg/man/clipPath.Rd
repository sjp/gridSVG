\name{Clipping Paths}
\alias{createClippingPath}
\alias{clipPath}
\title{
  Create the definition of a clipping path.
}
\description{
  A feature of SVG is that elements can be clipped to by a variety of
  different types of shapes. In grid, clipping is restricted only to the
  rectangular region defined by a viewport, this restriction is not
  present in SVG. The purpose of these functions is to create the
  definition of a clipping path so that it can be referred to by grobs
  drawn by gridSVG.
}
\usage{
createClippingPath(grob = NULL, vp = NULL)
clipPath(label, clippath = NULL, ...)
}

\arguments{
  \item{grob}{
    A grid grob or tree of grobs.
  }
  \item{vp}{
    A grid viewport object. If not \code{NULL}, this will be pushed into
    prior to drawing the \code{grob}.
  }
  \item{label}{
    A character identifier for the definition.
  }
  \item{clippath}{
    A \code{"clipping.path"} object, produced by the
    \code{createClippingPath} function.
  }
  \item{\dots}{
    Arguments to be passed onto \code{createClippingPath}.
  }
}
\details{

  The clipping path is drawn off-screen on a new device. This device has
  the same dimensions as the current graphics device. The \code{grob}
  and \code{vp} that have been given are then drawn on this new device.

  Most grobs can be drawn to this device but there are some limitations
  on what will actually be used for clipping. In general though,
  anything that is drawn to the new device will have the union of its
  drawn regions become the region that will be clipped to.

  The limitations are as follows:

  \itemize{
    \item A viewport with a clipping path will no longer clips its
    contents. However, its clipping region will remain. This means that
    the clipping region for a viewport will become the union of its
    contents and the viewport clipping region itself, instead of just
    the viewport clipping region.

    \item No pointGrobs are able to be used for clipping.

    \item Any operations that apply to containers (e.g. gpars,
    garnishing, animation), will no longer work. Any operations that are
    not applied to groups are unaffected. This affects in particular
    viewports, gTrees, and the familiar gridSVG grob grouping that
    occurs.

  }
}
\value{
  For \code{createClippingPath}, a \code{clipping.path} object. None for
  \code{clipPath}.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.clipPath}}
}
