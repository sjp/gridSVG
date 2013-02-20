
library(grid)
library(gridSVG)
dev.new(width=6, height=6)
grid.rect(gp=gpar(col="black", fill=NA))
grid.multipanel(vp=viewport(w=.7, h=.7, gp=gpar(col="black", fill=NA)),
                newpage=FALSE)
gridToSVG("plot.svg")
dev.off()
