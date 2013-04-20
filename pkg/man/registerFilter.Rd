\name{registerFilter}
\alias{registerFilter}
\title{
  Create the definition a filter effect.
}
\description{
  A feature of SVG is that elements can be filtered using filter effects
  defined somewhere in the document. The purpose of this function is to
  create the definition of a filter effect so that it can be referred to
  by grobs drawn by gridSVG.
}
\usage{
registerFilter(label, filter)
}
\arguments{
  \item{label}{
    A character identifier for the definition.
  }
  \item{filter}{
    A \code{filter} object, produced by the \code{\link{filterEffect}}
    function.
  }
}
\details{
  When registering a filter, all locations and dimensions that filter
  effects refer to become fixed.
}
\value{
  None.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.filter}}, \code{\link{filterEffect}}.
}
