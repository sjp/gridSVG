\name{feComponentTransfer}
\alias{feComponentTransfer}
\alias{addComponentFunction}
\alias{transferFunction}
\title{
  Perform Colour Component-wise Remapping.
}
\description{
  This filter primitive performs component-wise remapping of data by
  taking a colour transfer function, and applying that to the set of
  RGBA colour components.

  It allows operations like brightness adjustment, contrast adjustment,
  colour balance or thresholding.

  The calculations are performed on non-premultiplied colour values. If
  the input graphics consists of premultiplied colour values, those
  values are automatically converted into non-premultiplied colour
  values for this operation. (Note that the undoing and redoing of the
  premultiplication can be avoided if alpha transfer function is the
  identity transform and all alpha values on the source graphic are set
  to 1.)
}
\usage{
feComponentTransfer(input = NA, transfers = NULL, ...)
addComponentFunction(ct, channel = c("R", "G", "B", "A"), func)
transferFunction(type = c("identity", "table", "discrete",
                          "linear", "gamma"),
                 tableValues = numeric(),
                 slope = 1, intercept = 0,
                 amplitude = 1, exponent = 1, offset = 0)
}
\arguments{
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{transfers}{
    A named list of \code{transfer.function} objects (produced by
    \code{\link{transferFunction}}). The name for each element of the
    list should be one of \code{R}, \code{G}, \code{B} or \code{A}.
  }
  \item{\dots}{
    Further arguments to be passed onto \code{\link{fe}}.
  }
  \item{ct}{
    An \code{fe.component.transfer} object, produced by
    \code{\link{feComponentTransfer}}.
  }
  \item{channel}{
    The colour channel that \code{func} will be applied to.
  }
  \item{func}{
    A \code{transfer.function} object, produced by
    \code{\link{transferFunction}}.
  }
  \item{type}{
    Indicates the type of component transfer function. The type of
    function determines the applicability of the other arguments.
  }
  \item{tableValues}{
    When \code{type} is \code{"table"}, this is a list of values which
    define the lookup table.
  }
  \item{slope}{
    When \code{type} is \code{"linear"}, the slope of the linear
    function.
  }
  \item{intercept}{
    When \code{type} is \code{"linear"}, the intercept of the linear
    function.
  }
  \item{amplitude}{
    When \code{type} is \code{"gamma"}, the amplitude of the gamma
    function.
  }
  \item{exponent}{
    When \code{type} is \code{"gamma"}, the exponent of the gamma
    function.
  }
  \item{offset}{
    When \code{type} is \code{"gamma"}, the offset of the gamma
    function.
  }
}
\details{
  For more information about this primitive, consult the references to
  the SVG specification.
}
\value{
  For \code{feComponentTransfer}, an \code{fe.component.transfer}
  object.

  For \code{addComponentFunction}, none.

  For \code{transferFunction}, a \code{transfer.function} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feComponentTransferElement},
  \url{http://www.w3.org/TR/SVG/filters.html#feFuncRElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
