\name{grid.script}
\alias{grid.script}
\alias{scriptGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a grid grob containing an SVG script}
\description{
  Creates a script object which is a normal grid grob
  containing an SVG script. Useful in conjunction with
  \code{gridToSVG}, to produce an SVG document with
  script elements.
}
\usage{
scriptGrob(script=NULL, filename=NULL, type="application/ecmascript",
           inline=FALSE, name=NULL)
grid.script(...)
}
\arguments{
  \item{script}{ A character value specifying script code. }
  \item{filename}{ The name of a file that contains script code. }
  \item{type}{ The type of the script code. }
  \item{inline}{ A logical specifying whether the script code from the
    file should be included inline or just referenced. }
  \item{name}{ A character value giving a name for the grob. }
  \item{\dots}{ Arguments to be passed into \code{scriptGrob}. }
}
\value{
  A script.grob object.
}
\author{ Paul Murrell }
\seealso{ \code{\link{gridToSVG}} }                           
\keyword{ dplot }% at least one, from doc/KEYWORDS

