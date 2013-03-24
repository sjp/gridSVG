\name{Retrieve Names Mapped to SVG IDs, CSS Selectors and XPath Expressions}
\alias{getSVGMappings}
\title{
  Retrieving Viewport, Grob, and Reference Names as SVG IDs, CSS
  Selectors and XPath Expressions
}
\description{
  This function gives us SVG IDs (or CSS selectors and XPath
  expressions) that have been created from a grob, viewport, or
  referenced name as a result of exporting to SVG.
}
\usage{
getSVGMappings(name, type, result = "id")
}
\arguments{
  \item{name}{
    A single element character vector. This should be the name of a grob
    or viewport (as determined by \code{type}) present as the grid plot
    was exported.
  }
  \item{type}{
    A single element character vector, must be one of \code{vp},
    \code{grob} or \code{ref}. This determines whether we are trying to
    get the IDs of a grob or a viewport or a referenced object like a
    fill pattern.
  }
  \item{result}{
    The type of output we want. \code{id} gives us SVG element
    IDs. \code{selector} gives us CSS selectors. \code{xpath} gives us
    XPath expressions.
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
  the case when \code{result} is specified as one of \code{selector} or
  \code{xpath}. These are targeted to match just the SVG element itself,
  nothing more.
}
\value{
  A character vector representing values that can target specific SVG
  output.
}
\author{
  Simon Potter
}
