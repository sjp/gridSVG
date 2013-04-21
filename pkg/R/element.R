# Functions for generating arbitrary SVG elements

elementGrob <- function(el, name = NULL, attrs = NULL, children = NULL,
                        vp = NULL, childrenvp = NULL,
                        asis = FALSE) {
    eg <- gTree(name = name, vp = vp,
                children = children, childrenvp = childrenvp,
                cl = "element")
    # Keeping copy of name because of asis.
    # If it's TRUE, we leave the id alone.
    # When FALSE, the resulting id attribute could get modified
    # by things like gTrees so that the name is a *path*.
    eg$asis <- asis
    eg$origname <- eg$name

    eg$el <- el
    eg$attrs <- if (is.null(attrs)) list() else attrs
    cl <- class(eg)
    class(eg) <- unique(c("element.grob", cl))
    eg
}

grid.element <- function(el, name = NULL, attrs = NULL, children = NULL,
                         vp = NULL, childrenvp = NULL,
                         asis = FALSE) {
    grid.draw(elementGrob(el, name, attrs, children, vp, childrenvp, asis))
}

devGrob.element.grob <- function(x, dev) {
  list(id = if (x$asis) x$origname
            else getID(x$name, "grob"),
       name = x$el,
       classes = x$classes,
       attrs = x$attrs)
}

# Unlike gTrees, we don't need a group for children because it
# complicates output, when we want clear output to SVG.
# Also, do *not* add gpars because they also complicate output,
# if we *really* want to do it, then just use the 'attrs' arg.
primToDev.element.grob <- function(x, dev) {
    devStartElement(devGrob(x, dev), NULL, dev)
    lapply(x$children, function(child) {
        grobToDev(child, dev)
    })
    devEndElement(x$name, dev)
}
