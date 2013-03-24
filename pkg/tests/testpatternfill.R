library(gridSVG)

pdf(file = NULL)
# We are just going to be drawing a cross for our pattern
crossGrob <- gTree(children = gList(
    linesGrob(),
    linesGrob(x = unit(0:1, "npc"), y = unit(1:0, "npc"),
              gp = gpar(lwd = 1))
))

# Call the pattern "cross"
# Using a small device size because the line widths
# will be too small otherwise
fillPattern("cross", grob = crossGrob,
            dev.width = 1, dev.height = 1)

grid.circle(name = "filledcircle")
# Applying the pattern semi-transparently to the circle
grid.patternFill("filledcircle", "cross", alpha = 0.5)
gridToSVG("pattern-test.svg")
dev.off()

