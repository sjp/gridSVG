\name{listSVGDefinitions}
\alias{listSVGDefinitions}
\title{
  List All Reference Definitions
}
\description{
  Returns a listing of the labels given to reference definitions.
}
\usage{
listSVGDefinitions(print = TRUE)
}
\arguments{
  \item{print}{
    If \code{TRUE}, prints the listing of reference definitions.
  }
}
\details{
  When definitions of referenced content are stored in gridSVG via any
  of the \code{register*} functions
  (e.g. \code{\link{registerPatternFill}}), we can use this function to
  show us all of the labels given when content is registered.
}
\value{
  A data frame, returned invisibly.
}
\author{
  Simon Potter
}
