\name{gaussianBlur}
\alias{gaussianBlur}
\title{
  Define a Gaussian Blur.
}
\description{
  Allows the creation of a Gaussian blur filter which can be referenced
  by grid grobs.
}
\usage{
gaussianBlur(label, sd = 1)
}
\arguments{
  \item{label}{
    A character identifier for the blur definition.
  }
  \item{sd}{
    A numeric vector of one or two elements. Specifies the "blurriness"
    of the Gaussian blur. If this is two elements, it specifies the
    standard deviation for the x and y dimensions respectively. If a
    single element, defines the standard deviation for both dimensions.
  }
}
\value{
  None.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{grid.gaussianBlur}}
}
