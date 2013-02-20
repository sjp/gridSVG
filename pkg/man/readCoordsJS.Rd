\name{Import JS as JSON}
\alias{readCoordsJS}
\title{
  Importing JavaScript coordinate information as JSON.
}
\description{
  This function reads in a JavaScript file and transforms it into JSON
  text.
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
  for parsing by \code{\link{fromJSON}}.
}
\value{
  A single element character vector is returned, representing a JSON
  object.
}
\author{
  Simon Potter
}
