\name{getSVGFonts}
\alias{getSVGFonts}
\alias{setSVGFonts}
\title{
  Manage SVG fonts
}
\description{
  These functions control the SVG font stacks that are used when
  exporting text to SVG.
}
\usage{
getSVGFonts()
setSVGFonts(fontStacks)
}
\arguments{
  \item{fontStacks}{A list of font stacks (typically the modified result
    from \code{getSVGFonts()}).
  }
}
\details{
  \code{getSVGFonts()} returns a list of three font stacks called
  \code{serif}, \code{sans}, and \code{mono}.  The user can modify the
  values in each stack and then reset the stacks by calling
  \code{setSVGFonts()} (a default value will always be forced at the
  end of each font stack).
}
\value{
  A list (for \code{getSVGFonts()}).
}
\author{
Simon Potter
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
