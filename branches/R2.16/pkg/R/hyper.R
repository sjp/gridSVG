
# FIXME:  What should happen if a grob has BOTH group and individual hrefs?
#         Is that an error?

hyperlinkGrob <- function(x, href, group=TRUE) {
    if (group) 
        x$groupLinks <- href
    else
        x$links <- href
    class(x) <- unique(c("linked.grob", class(x)))
    x
}

grid.hyperlink <- function(path, href, group=TRUE) {
    x <- grid.get(path)
    x <- hyperlinkGrob(x, href, group)
    grid.set(path, x, redraw=FALSE)
}

link <- function(x) {
    UseMethod("link")
}

link.grob <- function(x) {
    href <- x$links
    if (!is.null(href)) {
        n <- length(href)
        if (is.null(names(href)))
            names(href) <- subGrobName(x$name, 1:n)
    }
    groupHref <- x$groupLinks
    if (!is.null(groupHref))
        names(groupHref) <- x$name
    c(href, groupHref)
}

# A hopefully useful default for gTrees
link.gTree <- function(x, ...) {
    href <- x$links
    if (!is.null(href)) {
        n <- length(href)
        if (is.null(names(href)))
            names(href) <- (x$childrenOrder)[1:n]
    }
    groupHref <- x$groupLinks
    if (!is.null(groupHref))
        names(groupHref) <- x$name
    c(href, groupHref)
}

# Set the 'links' slot in the device
# The catsvg() function in svg.R picks this up
# and matches links to element names
primToDev.linked.grob <- function(x, dev) {
    dev@links <- link(x)
    NextMethod()
}

# gridToDev method for linked.grob objects
# grobToDev.linked.grob <- function(x, dev) {
#   svgStartLink(x$href, dev@dev)
#   NextMethod()
#   svgEndLink(dev@dev)
# }
