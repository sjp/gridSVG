\name{gridSVG.newpage}
\alias{gridSVG.newpage}
\title{
  Move to a New Page on a gridSVG Device
}
\description{
  This function erases the current device or moves to a new page. In
  addition, it clears any definitions of referenced content defined by
  gridSVG.
}
\usage{
gridSVG.newpage(wipeRefs = TRUE, recording = TRUE)
}
\arguments{
  \item{wipeRefs}{
    A logical value that determines whether referenced content should be
    deleted.
  }
  \item{recording}{
    A logical value to indicate whether the new-page operation should be
    saved onto the Grid display list.
  }
}
\details{
  When creating a gridSVG image, it is possible to create referenced
  content. An example is pattern fills. This function should be used in
  order to remove the definitions of referenced content.
}
\value{
  None.
}
\author{
  Simon Potter
}