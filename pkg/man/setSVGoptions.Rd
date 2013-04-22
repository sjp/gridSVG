\name{setSVGoptions}
\alias{setSVGoptions}
\alias{getSVGoption}
\alias{getSVGoptions}
\title{ Get and Set Global Options }
\description{
  Provides access to a predefined set of global options for the
  \pkg{gridSVG} package.
}
\usage{
getSVGoption(name)
getSVGoptions()
setSVGoptions(...)
}
\arguments{
  \item{name}{ The name of one option. }
  \item{...}{
    Named arguments giving a name, value pair for a new option setting.
  }
}
\details{
  The options currently available are:
  \itemize{
    \item \code{id.sep} which controls the separator used between the
    grob name and the suffix number when \pkg{gridSVG} generates
    id values for SVG elements.
    \item \code{gPath.sep} which controls the separator used between
    elements of a grid \code{gPath}.
    \item \code{vpPath.sep} which controls the separator used between
    elements of a grid \code{vpPath}.
  }
}
\value{
  \code{getSVGoption()} returns at most one option setting.
  \code{getSVGoptions()} returns all option settings.
  \code{setSVGoptions()} returns a list of previous option settings
  for the options that were changed.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.export}} }
\keyword{ dplot }
