library(gridSVG)

dev.new(width=6, height=6)
grid.rect(name = "mainrect")
grid.comment("mainrect", "This is a comment")
gridToSVG("comment-test.svg")
dev.off()
