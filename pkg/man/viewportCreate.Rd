\name{viewportCreate}
\alias{viewportCreate}
\title{
  Recreate a viewport from imported coordinate information.
}
\description{
  Creates a viewport object that is positioned in the same location as a
  previously exported viewport.

  The purpose of this function is so that we can recreate content for
  later manipulation.
}
\usage{
viewportCreate(vpname, newname = NULL,
               vpPath.sep = getSVGoption("vpPath.sep"))
}
\arguments{
  \item{vpname}{
    The name of the viewport to be recreated, as stored in
    coordinate information. This is most likely a viewport path.
  }
  \item{newname}{
    The name that is going to be assigned to the viewport as it is
    re-created. If this parameter is \code{NULL}, then the name is taken
    to be the last viewport in listed in \code{vpname} (because it is
    usually a viewport path).
  }
  \item{vpPath.sep}{
    The viewport path separator that was used for \code{vpname}.
  }
}
\details{
  In order to use this function, coordinate information must be
  available to \code{gridSVG}. This means that viewport information must
  be imported using \code{\link{gridSVGCoords}}.

  The \code{ROOT} viewport must also have coordinate information
  imported because the created viewport is positioned relative to this.
}
\value{
  A viewport object.
}
\author{
  Simon Potter
}
\examples{\dontrun{
  grid.newpage()

  # Pushing a new VP to draw a rect within
  pushViewport(viewport(x = unit(0.3, "npc"), y = unit(0.2, "npc"),
                        width = unit(0.1, "npc"), height = unit(0.3, "npc"),
                        xscale = c(0, 20), yscale = c(0, 10),
                        name = "testVP"))
  grid.rect()
  grid.export("create-test.svg", exportCoords = "file")

  # Importing coordinate information
  gridSVGCoords(readCoordsJS("create-test.svg.coords.js"))

  # This should appear to be the same rect
  grid.newpage()
  pushViewport(viewportCreate("testVP.1"))
  grid.rect()

  # Let's see if the scales are accurate, should be:
  # xscale: [0, 20]
  # yscale: [0, 10]
  current.viewport()$xscale
  current.viewport()$yscale
}}
