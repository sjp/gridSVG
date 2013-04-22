library(gridSVG)

dev.new(width=6, height=6)
grid.element("test")
grid.element("testParent",
             children = gList(commentGrob("This is a child comment"),
                              elementGrob("firstChild"),
                              rectGrob(),
                              elementGrob("thirdChild",
                                          attrs = list(anAttrib = "value"))))
grid.export("element-test.svg")
dev.off()
