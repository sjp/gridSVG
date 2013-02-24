library(gridSVG)

dev.new(width=6, height=6)
grid.rect(name = "mainrect")
grid.comment("This is a comment", "mainrect")
gridToSVG("comment-test.svg")
dev.off()
