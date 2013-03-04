\name{Retrieve Names Mapped to SVG IDs}
\alias{getSVGMappings}
\title{
  Retrieving Viewport and Grob Names as SVG IDs
}
\description{
  This function gives us the SVG IDs that have been created from a grob
  or viewport name as a result of exporting to SVG.
}
\usage{
getSVGMappings(name, type, result = "id", index = NULL)
}
\arguments{
  \item{name}{
    A single element character vector. This should be the name of a grob
    or viewport (as determined by \code{type}) present as the grid plot
    was exported.
  }
  \item{type}{
    A single element character vector, must be one of \code{vp} or
    \code{grob}. This determines whether we are trying to get the IDs of
    a grob or a viewport.
  }
  \item{result}{
    The
  }
  \item{index}{
    Subsets the resulting IDs by a vector of indices if
    required. \code{NULL} assumes all IDs are to be mapped.
  }
}
\details{
  In order to generate unique names for SVG IDs, gridSVG output will not
  produce the same names as are visible on the grid display list. This
  function retrieves the SVG IDs associated with grob and viewport
  names. To use this function first requires importing mapping
  information, see \code{\link{gridSVGMappings}}.

  To make using results easier with existing JavaScript libraries and R
  packages, CSS selectors and XPath expressions can be returned. This is
  the case when \code{result} is specified as one of \code{"selector"}
  or \code{"xpath"}. These are targeted to match just the SVG element
  itself, nothing more.
}
\value{
  A character vector representing SVG IDs.
}
\author{
  Simon Potter
}
