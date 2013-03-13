# Setting up an environment for use within gridSVG
.gridSVGEnv <- new.env()

# Setting a clipping level for clipGrobs.
# Allows popping viewports to work correctly if we know many
# SVG groups we need to "pop".
assign("clipLevel", 0, envir = .gridSVGEnv)
