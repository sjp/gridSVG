\name{grobToDev}
\alias{grobToDev}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Convert a grob to device calls
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  This function is used to make calls to a device to
  draw a grob.  It is generic so new grob classes can
  write their own methods.
}
\usage{
grobToDev(x, dev)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A grob.
%%     ~~Describe \code{x} here~~
}
  \item{dev}{  A graphics device.
%%     ~~Describe \code{dev} here~~
}
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
  This function is not called directly by the user.
  It is exposed so that new grob classes can easily write
  their own methods which call existing methods for
  standard grobs.

  The difference between this function and \code{primToDev()} is
  that this one takes care of setting up coordinate systems based on
  the grid viewports so that
  SVG output is positioned correctly, then it calls \code{primToDev()}
  to produce the actual SVG elements.
}
\author{
  %%  ~~who you are~~
  Paul Murrell
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
