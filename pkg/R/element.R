# Functions for generating generic SVG elements

elementGrob <- function(el, name = NULL, attrs = NULL, children = NULL,
                        vp = NULL, childrenvp = NULL) {
    ng <- nullGrob(name = name, vp = vp)
    # Fix name to be an elementGrob name instead of nullGrob
    if (is.null(name))
        ng$name <- gsub("null", "element", ng$name)
    ng$el <- el
    ng$attrs <- if (is.null(attrs)) list() else attrs
    ng$children <- children
    ng$childrenvp <- childrenvp
    cl <- class(ng)
    class(ng) <- unique(c("element.grob", cl))
    ng
}


grid.element <- function(el, name = NULL, attrs = NULL, children = NULL,
                         vp = NULL, childrenvp = NULL) {
    grid.draw(elementGrob(el, name, attrs, children, vp, childrenvp))
}

# Same as gTree because we might want to place elements in specific
# viewports/<g>'s (due to gpars)
# Fortunately if we have them unnecessarily, it doesn't affect any
devGrob.element.grob <- function(x, dev) {
  list(id = getID(x$name, "grob"),
       name = x$el,
       attrs = x$attrs)
}

# output because SVG <g>'s do not draw anything
grobToDev.element.grob <- function(x, dev) {
  depth <- enforceVP(x$vp, dev)
  if (!is.null(x$childrenvp)) {
    pushViewport(x$childrenvp)
    upViewport(grid:::depth(x$childrenvp))
  }
  primToDev(x, dev)
  unwindVP(x$vp, depth, dev)
}


# Unlike gTrees, we don't need a group for children because it
# complicates output, when we want clear output to SVG.
# Also, do *not* add gpars because they also complicate output,
# if we *really* want to do it, then just use the 'attrs' arg.
primToDev.element.grob <- function(x, dev) {
    devOpenElement(devGrob(x, dev), NULL, dev)
    lapply(x$children, function(child) {
        grobToDev(child, dev)
    })
    devEndElement(x$name, dev)
}
