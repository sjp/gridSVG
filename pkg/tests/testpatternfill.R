library(gridSVG)

pdf(file = NULL)
grid.newpage()
# We are just going to be drawing a cross for our pattern
crossGrob <- gTree(children = gList(
    linesGrob(),
    linesGrob(x = unit(0:1, "npc"), y = unit(1:0, "npc"),
              gp = gpar(lwd = 1))
))

# Call the pattern "cross"
# Using a small device size because the line widths
# will be too small otherwise
patternFill("cross", grob = crossGrob,
            dev.width = 1, dev.height = 1)

grid.circle(name = "filledcircle")
# Applying the pattern semi-transparently to the circle
grid.patternFill("filledcircle", "cross", alpha = 0.5)
gridToSVG("pattern-test.svg")
dev.off()

# Now lets create a new pattern that uses the existing pattern
# but much larger
pdf(file = NULL)
grid.newpage()
patternFillRef("bigcross", "cross",
               width = 0.3, height = 0.3)
grid.circle(name = "filledcircle")
grid.patternFill("filledcircle", "bigcross", alpha = 0.5)
gridToSVG("pattern-test-ref.svg")
dev.off()
