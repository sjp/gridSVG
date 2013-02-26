\name{grid.element}
\alias{grid.element}
\alias{elementGrob}
\title{ Create a grid grob representing an SVG element }
\description{
  Creates a \code{element.grob} object which is a grid
  \code{\link{gTree}}, representing an SVG element. Useful in
  conjunction with \code{gridToSVG}, to produce an SVG document with
  elements inserted at particular points. The element (and its children)
  are inserted at the point where the grob is \dQuote{drawn}.
}
\usage{
grid.element(el, name = NULL, attrs = NULL, children = NULL,
             vp = NULL, childrenvp = NULL, asis = FALSE)
elementGrob(el, name = NULL, attrs = NULL, children = NULL,
            vp = NULL, childrenvp = NULL, asis = FALSE)
}
\arguments{
  \item{el}{
    The name of the SVG element to create, e.g. "rect".
  }
  \item{name}{
    A character identifier.
  }
  \item{attrs}{
    A list, where the names are SVG attribute names, and values are the
    values given to the SVG attributes.
  }
  \item{children}{
    A gList object containing children of this element (if any).
  }
  \item{vp}{ A viewport object to draw within. }
  \item{childrenvp}{
    A viewport object to use for the children of the element grob.
  }
  \item{asis}{
    If \code{TRUE}, SVG id attributes will be unmodified so that we can
    easily refer to the generated elements.
  }
}
\value{
  An \code{element.grob} object.
}
\author{ Simon Potter }
\seealso{ \code{\link{gridToSVG}} }
\keyword{ dplot }
