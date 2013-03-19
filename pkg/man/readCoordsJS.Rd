\name{Import Coordinate JS}
\alias{readCoordsJS}
\title{
  Importing JavaScript coordinate information.
}
\description{
  This function reads in a JavaScript file and transforms it into JSON
  text. This text is then transformed into a list that can be used in
  conjunction with \code{\link{gridSVGCoords}}.
}
\usage{
readCoordsJS(filename)
}
\arguments{
  \item{filename}{
    A character vector that represents a file name. This file should be
    a JavaScript file containing coordinate information produced by
    \code{\link{gridToSVG}}.
  }
}
\details{
  In order to use the \code{\link{fromJSON}} function to parse JSON text,
  the JavaScript file produced by \code{\link{gridToSVG}} needs to be
  transformed. It needs to transform from being an assignment of an object
  literal to simply the object literal itself.

  This function performs that task by producing a valid JSON string ready
  for parsing by \code{\link{fromJSON}}. It then returns the parsed list.
}
\value{
  A list of coordinate information.
}
\author{
  Simon Potter
}
