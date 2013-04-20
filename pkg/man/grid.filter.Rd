\name{grid.filter}
\alias{grid.filter}
\alias{filterGrob}
\title{
  Associate a filter effect with a grid grob.
}
\description{
  Creates a \code{filtered.grob} object which is a normal grid grob,
  with a filter effect applied to it Used in conjunction with
  \code{\link{registerFilter}}, to produce an SVG document containing
  graphical elements with filter effects.
}
\usage{
grid.filter(path, filter = NULL, label = NULL,
            group = TRUE, redraw = FALSE,
            strict = FALSE, grep = FALSE, global = FALSE)
filterGrob(x, filter = NULL, label = NULL, group = TRUE)
}
\arguments{
  \item{x}{
    A grob to filter.
  }
  \item{path}{
    A grob path specifying a drawn grob.
  }
  \item{filter}{
    A \code{filter} object, provided by the \code{\link{filterEffect}}
    function. Provides the definition of a filter effect that will be
    applied to \code{x} or \code{path}.
  }
  \item{label}{
    A label that is associated with a definition of a filter effect.
    This is the label used to create a filter effect definition with
    \code{registerFilter}.
  }
  \item{group}{
    A logical vector that indicates whether the filter effect should be
    applied to the overall parent group for the relevant SVG element, or
    to individual SVG elements.
  }
  \item{redraw}{
    A logical value to indicate whether to redraw the grob.
  }
  \item{strict}{
    A boolean indicating whether the \code{path} must be matched
    exactly.
  }
  \item{grep}{
    Whether the \code{path} should be treated as a regular expression.
  }
  \item{global}{
    A boolean indicating whether the function should affect just the
    first match of the \code{path}, or whether all matches should be
    affected.
  }
}
\details{
  If \code{label} is specified, uses a filter effect that has been supplied to
  \code{registerFilter}. If \code{filter} is specified it will be used as
  the filter effect applied to each grob. If both are specified, it will
  attempt to define the filter effect with the given label, as well as
  applying the filter effect to the appropriate grobs.
}
\value{
  A \code{filtered.grob} object (for \code{filterGrob}).
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{registerFilter}}, \code{\link{filterEffect}}.
}
