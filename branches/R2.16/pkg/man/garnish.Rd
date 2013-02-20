\name{garnish}
\alias{garnish}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Convert animation specifications to SVG elements.
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  This function is used to generate a list of SVG attributes
  based on information on a grob.
  It is generic so new grob classes can
  write their own methods.
}
\usage{
garnish(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A grob.
%%     ~~Describe \code{x} here~~
}
  \item{...}{ For future use.
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
