\name{animUnit}
\alias{animUnit}
\alias{animValue}
\alias{as.animUnit}
\alias{as.animValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Generate a set of animation values.
}
\description{
  These functions can be used to generate a set of values
  for use with \code{grid.animate()} to animate some
  feature of a grob.
}
\usage{
animUnit(x, timeid = NULL, id = NULL)
animValue(x, timeid = NULL, id = NULL)
as.animUnit(x, ...)
as.animValue(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A set of animation values.  Could be a numeric vector, a character
    vector, a unit vector, a matrix, a list of units.
  }
  \item{timeid}{
    A vector that associates each value of \code{x} with a time point.
  }
  \item{id}{
    A vector that associates each value of \code{x} with a different
    (numeric) identifier.
  }
  \item{\dots}{ For future use. }
}
\details{
  A set of animation values is ultimately either a numeric or character
  vector OR a unit vector.  Subsets of the animation values can be
  defined per time point, or per identifier, or both.

  The \code{as} functions allow animation values to be specified as
  matrices or lists, which are converted to formal animation value sets.
  The \code{grid.animate()} function calls these functions so the
  conversion typically happens automatically.

  These functions should only have to be called directly in relatively
  complex cases where multiple values need to be specified per time
  point AND per identifier.
}
\value{
  An animUnit or animValue object.
}
\author{
  Paul Murrell
}
\seealso{
\code{\link{grid.animate}}
}
\examples{
animValue(c("visible", "hidden"))
animUnit(unit(1:24, "in"),
         timeid=rep(1:3, each=8),
         id=rep(1:2, 12))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }% __ONLY ONE__ keyword per line
