\name{Filter Inputs}
\alias{filterInputs}
\title{
  Identifies input for a filter effect primitive.
}
\description{
  How to use and identify inputs for filter effect primitives.
}
\section{Filter Inputs}{

  The value chosen for a filter effect primitive can be either one of
  six keywords or can be a string which matches a previous \code{result}
  attribute value within the same filter effect container. If no value
  is provided and this is the first filter effect primitive, then the
  input will be \code{SourceGraphic}. If no value is provided and this
  is a subsequent filter effect primitive, then this filter effect
  primitive will use the result from the previous filter primitive as
  its input.

  If the value for \code{result} appears multiple times within a given
  filter container, then a reference to that result will use the closest
  preceding filter primitive with the given value for the \code{result}
  results. Forward references to results are an error and will not draw.

  Definitions for the seven possible options:

  \itemize{

    \item \code{SourceGraphic} This keyword represents the appearance of
    grobs before they are being filtered. For raster effects filter
    primitives, the grobs will be rasterized into an initially clear
    RGBA raster in image space. Pixels left untouched by the original
    graphic will be left clear. The image is specified to be rendered in
    linear RGBA pixels. The alpha channel of this image captures any
    anti-aliasing specified by SVG. (Since the raster is linear, the
    alpha channel of this image will represent the exact percent
    coverage of each pixel.)

    \item \code{SourceAlpha} This keyword represents the appearance of
    grobs before they are being filtered. \code{SourceAlpha} has all of
    the same rules as \code{SourceGraphic} except that only the alpha
    channel is used. The input image is an RGBA image consisting of
    implicitly black color values for the RGB channels, but whose alpha
    channel is the same as \code{SourceGraphic}. If this option is used,
    then some implementations might need to rasterize the graphics
    elements in order to extract the alpha channel.

    \item \code{BackgroundImage} This keyword represents an image
    snapshot of the canvas under the filter region at the time that the
    referring grob is being filtered.

    \item \code{BackgroundAlpha} Same as \code{BackgroundImage} except
    only the alpha channel is used.

    \item \code{FillPaint} This keyword represents the value of the
    \code{fill} property on the grob being filtered. The
    \code{FillPaint} image has conceptually infinite extent. Frequently
    this image is opaque everywhere, but it might not be if the "paint"
    itself has alpha, as in the case of a gradient or pattern which
    itself includes transparent or semi-transparent parts.

    \item \code{StrokePaint} This keyword represents the value of the
    \code{col} property on the grob being filtered. The
    \code{StrokePaint} image has conceptually infinite
    extent. Frequently this image is opaque everywhere, but it might not
    be if the "paint" itself has alpha, as in the case of a gradient or
    pattern which itself includes transparent or semi-transparent parts.

    \item The result of any filter effect operation. This is the name
    that has been given to the \code{result} argument of a filter
    primitive.

  }
}
\author{
  Simon Potter
}
\references{
  \url{http://www.w3.org/TR/SVG/filters.html#FilterPrimitiveInAttribute}
}
