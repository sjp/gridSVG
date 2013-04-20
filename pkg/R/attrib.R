
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

grid.garnish <- function(path, ..., group=TRUE, redraw = FALSE,
                         strict=FALSE, grep=FALSE, global=FALSE) {
    grobApply(path, function(path) {
        grid.set(path, garnishGrob(grid.get(path), ..., group=group),
                 redraw = redraw)
    }, strict = strict, grep = grep, global = global)
    invisible()
}

garnish <- function(x, ...) {
    UseMethod("garnish")
}

# This is intended to handle all basic graphical primitives
garnish.grob <- function(x, ...) {
    x$name <- getID(x$name, "grob", FALSE)
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
    x$name <- getID(x$name, "grob", FALSE)
    c(lapply(x$attributes,
             function(attr) {
                 n <- length(attr)
                 if (is.null(names(attr)))
                     names(attr) <- sapply((x$childrenOrder)[1:n],
                                           function(x) getID(x, "grob", FALSE))
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

# Custom function for applying hyperlinks or garnishing to a set of grobs
grobApply <- function(path, FUN, ...,
                      strict = FALSE, global = FALSE, grep = FALSE) {
    if (! inherits(path, "gPath"))
        path <- grid:::gPathDirect(path)
    depth <- grid:::depth(path)
    grep <- rep(grep, length.out = depth)
    if (! strict)
        grep <- grep[1]

    # Get each piece of the path as a sequential char vector
    pathPieces <-
        if (! is.null(path$path))
            c(strsplit(path$path, grid:::.grid.pathSep)[[1]], path$name)
        else
            path$name

    dl <- grid.ls(print = FALSE)
    # Limit our search only to grobs whose depth matches ours
    # For not strict, we're only looking at the grob names, so all
    # depths apply.
    matchingDepths <- if (! strict) 1:length(dl$name)
                      else which((dl$gDepth + 1) == depth)
    if (! length(matchingDepths))
        return()

    nMatches <- 0
    searchMatches <- vector("list", length(matchingDepths))
    # For each name of the correct path length
    for (i in matchingDepths) {
        dlPathPieces <-
            if (! is.null(path$path))
                c(strsplit(dl$gPath[i], grid:::.grid.pathSep)[[1]],
                  dl$name[i])
            else
                dl$name[i]
        matches <- logical(depth)
        if (! strict) {
            # If this is not strict, check only the *names*, ignore path
            matches <- if (grep) grepl(path$name, dl$name[i])
                       else path$name == dl$name[i]
        } else {
            # Check whether we need to grep this level or not, attempt match
            for (j in 1:depth) {
                matches[j] <-
                    if (grep[j])
                        grepl(pathPieces[j], dlPathPieces[j])
                    else
                        pathPieces[j] == dlPathPieces[j]
            }
        }
        # We have found a grob
        if (all(matches)) {
            if (! global) {
                # Returning early to avoid further searching
                FUN(do.call("gPath", list(dlPathPieces)), ...)
                return()
            } else {
                nMatches <- nMatches + 1
                searchMatches[[nMatches]] <- do.call("gPath", list(dlPathPieces))
            }
        }
    }

    if (! nMatches)
        return()

    # We may have allocated a list too large earlier,
    # subset to only matching results
    searchMatches <- searchMatches[1:nMatches]

    # Now that we have all of the grobs, lets try and get them,
    # and then apply a function to each of them
    for (i in 1:nMatches)
        FUN(searchMatches[[i]], ...)

    # Ensure nothing is returned
    invisible()
}
