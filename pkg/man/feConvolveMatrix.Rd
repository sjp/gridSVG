\name{feConvolveMatrix}
\alias{feConvolveMatrix}
\title{
  Apply a matrix convolution filter effect.
}
\description{
  A convolution combines pixels in the input image with neighbouring
  pixels to produce a resulting image. A wide variety of imaging
  operations can be achieved through convolutions, including blurring,
  edge detection, sharpening, embossing and beveling.
}
\usage{
feConvolveMatrix(input = NA, order = 3,
                 kernelMatrix = matrix(),
                 divisor = 1, bias = 0,
                 targetX = 1, targetY = 1,
                 edgeMode = c("duplicate", "wrap", "none"),
                 kernelUnitLength = NA, preserveAlpha = FALSE,
                 ...)
}
\arguments{
  \item{input}{
    Identifies an input for this filter primtive. See
    \code{\link{filterInputs}}.
  }
  \item{order}{
    A numeric vector of length 1 or 2. Indicates the number of cells in
    each dimension for \code{kernelMatrix}. The values provided must be
    integers greater than zero. The first number (orderX), indicates the
    number of columns in the matrix. The second number (orderY),
    indicates the number of rows in the matrix. If this is a vector of
    length one then the number of rows is assumed to be same as the
    number of columns specified.
  }
  \item{kernelMatrix}{
    The kernel matrix for the convolution. The number of entries must
    correspond with the values given by \code{order}.
  }
  \item{divisor}{
    After applying the \code{kernelMatrix} to the input image to yield a
    number, that number is divided by \code{divisor} to yield the final
    destination colour value. A divisor that is the sum of all the
    matrix values tends to have an evening effect on the overall colour
    intensity of the result. It is an error to specify a divisor of
    zero. The default value is the sum of all values in kernelMatrix,
    with the exception that if the sum is zero, then the divisor is set
    to 1.
  }
  \item{bias}{
    After applying the \code{kernelMatrix} to the input image to yield a
    number and applying the \code{divisor}, the \code{bias} attribute is
    added to each component. One application of \code{bias} is when it
    is desirable to have 0.5 gray value be the zero response of the
    filter. The bias property shifts the range of the filter. This
    allows representation of values that would otherwise be clamped to 0
    or 1.
  }
  \item{targetX}{
    Determines the positioning in X of the convolution matrix relative
    to a given target pixel in the input image. The leftmost column of
    the matrix is column number zero. The value must be such that:
    \eqn{0 <= targetX < orderX}{0 <= targetX < orderX}. By default, the
    convolution matrix is centered in X over each pixel of the input
    image (i.e., \eqn{targetX = \lfloor orderX / 2 \rfloor}{targetX =
    floor( orderX / 2 ))}.
  }
  \item{targetY}{
    Determines the positioning in Y of the convolution matrix relative
    to a given target pixel in the input image. The topmost row of the
    matrix is row number zero. The value must be such that: \eqn{0 <=
    targetY < orderY}{0 <= targetY < orderY}. By default, the
    convolution matrix is centered in Y over each pixel of the input
    image (i.e., \eqn{targetY = \lfloor orderY / 2 \rfloor}{targetY =
    floor( orderY / 2))}.
  }
  \item{edgeMode}{
    Determines how to extend the input image as necessary with colour
    values so that the matrix operations can be applied when the kernel
    is positioned at or near the edge of the input image.

    \itemize{
      \item \code{"duplicate"} indicates that the input image is
      extended along each of its borders as necessary by duplicating the
      colour values at the given edge of the input image.

      \item \code{"wrap"} indicates that the input image is extended by
      taking the colour values from the opposite edge of the image.

      \item \code{"none"} indicates that the input image is extended
      with pixel values of zero for R, G, B and A.
    }
  }
  \item{kernelUnitLength}{
    The first number is the \code{dx} value. The second number is the
    \code{dy} value. If the \code{dy} value is not specified, it
    defaults to the same value as \code{dx}. Indicates the intended
    distance in current filter units (i.e., units as determined by the
    value of the filter effect container's \code{primitiveUnits})
    between successive columns and rows, respectively, in the
    \code{kernelMatrix}. By specifying value(s) for
    \code{kernelUnitLength}, the kernel becomes defined in a scalable,
    abstract coordinate system. If \code{kernelUnitLength} is not
    specified, the default value is one pixel in the offscreen bitmap,
    which is a pixel-based coordinate system, and thus potentially not
    scalable.
  }
  \item{preserveAlpha}{
    A value of \code{FALSE} indicates that the convolution will apply to
    all channels, including the alpha channel.

    A value of \code{TRUE} indicates that the convolution will only
    apply to the colour channels. In this case, the filter will
    temporarily unpremultiply the colour component values, apply the
    kernel, and then re-premultiply at the end.
  }
  \item{\dots}{
    Further arguments to be passed onto \code{\link{fe}}.
  }
}
\details{
  For more information about this primitive, consult the reference to
  the SVG specification.
}
\value{
  An \code{fe.convolve.matrix} object.
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#feConvolveMatrixElement}
}
\author{
  Simon Potter
}
\seealso{
  \code{\link{filterEffect}}, \code{\link{fe}}.
}
