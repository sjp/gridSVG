\name{Gradient Fills}
\alias{registerGradientFill}
\title{
  Create a definition of a gradient fill.
}
\description{
  A feature of SVG is that elements can be filled with a gradient that
  is defined somewhere in the document. The purpose of the
  \code{registerGradientFill} function is to create a definition of a
  gradient fill so that it can be referred to by grobs drawn by gridSVG.
}
\usage{
registerGradientFill(label, gradient)
}
\arguments{
  \item{label}{
    A character identifier for a gradient fill.
  }
  \item{gradient}{
    A gradient object filled with gradient stops. See
    \code{\link{linearGradient}} and \code{\link{radialGradient}}.
  }
}
\value{
  None.
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{linearGradient}}, \code{\link{radialGradient}},
  \code{\link{grid.gradientFill}}
}
