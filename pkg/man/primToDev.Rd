\name{primToDev}
\alias{primToDev}
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
primToDev(x, dev)
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
}
\author{
  %%  ~~who you are~~
  Paul Murrell
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
