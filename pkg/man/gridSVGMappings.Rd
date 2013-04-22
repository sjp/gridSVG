\name{Mapping Names to IDs}
\alias{gridSVGMappings}
\title{
  Mapping Viewport, Grob and Reference Names to SVG IDs
}
\description{
  This function is both a getter and a setter function for mapping
  information imported from a plot unknown to the current R session.
}
\usage{
gridSVGMappings(newmappings = NULL)
}
\arguments{
  \item{newmappings}{
    A named list mapping information, produced by
    \code{\link{grid.export}}.
  }
}
\details{
  In order to generate unique names for SVG IDs, gridSVG output will not
  produce the same names as are visible on the grid display list. This
  function will store and return mapping information. This is
  information on how names have been translated from their original
  grob/viewport names to their SVG IDs.

  Mapping information is stored as a list with 4 components, viewport
  mapping information, grob mapping information, reference mapping
  information and the ID separator used at the time of exporting.

  Viewport, grob, and reference mapping information is stored as the
  name of the object, paired with a vector of suffixes associated with
  these names. When combined with the ID separator, we can construct the
  SVG IDs that have been applied, given each name. Use
  \code{\link{getSVGMappings}} to do this.
}
\value{
  If \code{newmappings} is \code{NULL}, then we get back a named list
  representing name mapping information.

  If we pass the named list representing mapping information into the
  function, we get no output.
}
\author{
  Simon Potter
}
