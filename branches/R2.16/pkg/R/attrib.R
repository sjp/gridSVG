
# Add arbitrary SVG attributes to a grob

# This works by ...

# 1.  Enhancing a normal "grob" to make a "garnished.grob"
#     by adding an 'attributes' component

# 2.  Intercepting primToDev() calls (special method for "garnished.grob"s)
#     and setting an 'attrs' slot in the SVG device object.
#     The vectors of attribute values are given names
#     using the same name-generating function as is used in
#     normal primToDev() methods, subGrobName().
#     THEN the normal primToDev() method is called
#     (which will create SVG code).

# 3.  svg*() functions (like svgRect()) are sent the 'attrs' slot
#     from the SVG device.
#     These functions look in the attributes that they are given
#     and pull out the values where the 'name' corrsponds to
#     the 'id' of the element that they are drawing.

# The idea is that, if only ONE attribute value is specified, then the
# attribute value is given the name of the grob, so it will get picked
# up by the devStartGroup() call and the attribute will be set on the
# overall <g> element.
# Otherwise, the attribute values
# are named using subGrobName() so that they will get picked up by
# calls like devRect() and each individual SVG element will get the
# attribute (rather than the overall <g> element).

garnishGrob <- function(x, ..., group=TRUE) {
    cl <- class(x)
    # Should check that attributes are valid
    # Will need to be generic check with per-grob-type versions
    if (group) {
        x$groupAttributes <- c(x$groupAttributes, list(...))
    } else {
        x$attributes <- c(x$attributes, list(...))
    }
    class(x) <- unique(c("garnished.grob", cl))
    x
}

grid.garnish <- function(path, ..., grep=FALSE, redraw=FALSE) {
    grid.set(path, garnishGrob(grid.get(path, grep=grep), ...),
             grep=grep, redraw=redraw)
}

garnish <- function(x, ...) {
    UseMethod("garnish")
}

# This is intended to handle all basic graphical primitives
garnish.grob <- function(x, ...) {
    c(lapply(x$attributes,
             function(attr) {
                 n <- length(attr)
                 if (is.null(names(attr)))
                     names(attr) <- subGrobName(x$name, 1:n)
                 attr
             }),
      lapply(x$groupAttributes,
             function(attr, groupName) {
                 names(attr) <- x$name
                 attr
             }))
}

# A hopefully useful default for gTrees
garnish.gTree <- function(x, ...) {
    c(lapply(x$attributes,
             function(attr) {
                 n <- length(attr)
                 if (is.null(names(attr)))
                     names(attr) <- (x$childrenOrder)[1:n]
                 attr
             }),
      lapply(x$groupAttributes,
             function(attr, groupName) {
                 names(attr) <- x$name
                 attr
             }))                          
}

# NOTE that this has to be a primToDev() method
# NOT a grobToDev() method
# OTHERWISE, viewports will not be set up correctly
primToDev.garnished.grob <- function(x, dev) {
    dev@attrs <- garnish(x)
    NextMethod()
}
