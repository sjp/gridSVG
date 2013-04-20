# This file is concerned with the use of objects that need to be
# referenced, e.g. pattern fills, gradient fills, filters, etc.

drawDef <- function(def, dev) {
    UseMethod("drawDef")
}

# This function ensures that if we change the ID separator value between
# the time of definition and draw time, we still get the expected IDs.
assignRefIDs <- function() {
    refdefs <- get("refDefinitions", envir = .gridSVGEnv)
    for (i in seq_along(refdefs))
        refdefs[[i]]$id <- getLabelID(refdefs[[i]]$label)
    assign("refDefinitions", refdefs, envir = .gridSVGEnv)

    # Because the separators might have changed, ensure that the
    # usageTable has the correct (escaped) values for selectors and xpath
    ut <- get("usageTable", envir = .gridSVGEnv)
    inds <- which(ut$type == "ref")
    for (i in inds) {
        fullName <- paste(ut[i, "name"],
                          ut[i, "suffix"],
                          sep = getSVGoption("id.sep"))
        sel <- prefixName(escapeSelector(fullName))
        xp <- prefixName(escapeXPath(fullName))
        ut[i, "selector"] <- sel
        ut[i, "xpath"] <- xp
    }
    assign("usageTable", ut, envir = .gridSVGEnv)
}

flushDefinitions <- function(dev) {
    svgdev <- dev@dev

    refDefinitions <- get("refDefinitions", envir = .gridSVGEnv)
    if (! length(refDefinitions))
        return()

    # Keep copies of old tables because they will be modified when
    # we draw any children
    usageTable <- get("usageTable", envir = .gridSVGEnv)
    vpCoords <- get("vpCoords", envir = .gridSVGEnv)
    use.vpPaths <- get("use.vpPaths", envir = .gridSVGEnv)
    use.gPaths <- get("use.gPaths", envir = .gridSVGEnv)
    uniqueNames <- get("uniqueNames", envir = .gridSVGEnv)

    # Set required options -- we don't care about what the user
    # has specified at this point because they shouldn't be
    # touching any reference definitions
    assign("use.vpPaths", TRUE, envir = .gridSVGEnv)    
    assign("use.gPaths", TRUE, envir = .gridSVGEnv)
    assign("uniqueNames", TRUE, envir = .gridSVGEnv)
    
    # Begin creating definitions
    # First ensure we're under #gridSVG
    rootID <- prefixName("gridSVG")
    gridSVGNode <- getNodeSet(xmlRoot(svgDevParent(svgdev)),
                              paste0("//*[@id='", rootID, "']"))[[1]]
    svgDevChangeParent(gridSVGNode, svgdev)
    defs <- newXMLNode("defs", parent = svgDevParent(svgdev))
    svgDevChangeParent(defs, svgdev)

    # Check whether we have any dependent references, e.g. have a pattern
    # fill by reference in use but not the pattern itself. We need to ensure
    # that both are written out.
    n <- length(refDefinitions)
    rut <- get("refUsageTable", envir = .gridSVGEnv)
    for (i in 1:n) {
        def <- refDefinitions[[i]]
        if (isLabelUsed(def$label) &&
            ! is.null(def$refLabel) && ! isLabelUsed(def$refLabel))
            rut[rut$label == def$refLabel, "used"] <- TRUE
    }

    # Now trying to find out if there are trees of referenced content.
    for (i in 1:n) {
        used <- labelsUsed(refDefinitions[[i]])
        if (is.null(used))
            next
        flaggedLabels <- used %in% rut$label
        if (any(flaggedLabels))
            rut[flaggedLabels, "used"] <- TRUE
    }

    assign("refUsageTable", rut, envir = .gridSVGEnv) 

    # Now try drawing
    for (i in 1:n) {
        def <- refDefinitions[[i]]
        if (isLabelUsed(def$label))
            drawDef(def, dev)
    }

    # Resetting to original values
    assign("vpCoords", vpCoords, envir = .gridSVGEnv)
    assign("use.vpPaths", use.vpPaths, envir = .gridSVGEnv)
    assign("use.gPaths", use.gPaths, envir = .gridSVGEnv)
    assign("uniqueNames", uniqueNames, envir = .gridSVGEnv)

    # Reset ref usage table
    rut <- get("refUsageTable", envir = .gridSVGEnv)
    rut$used <- logical(nrow(rut))
    assign("refUsageTable", rut, envir = .gridSVGEnv)
    # Again for usage table 
    assign("usageTable", usageTable, envir = .gridSVGEnv)

    # Get out of defs
    svgDevChangeParent(xmlParent(defs), svgdev)
}

anyRefsDefined <- function() {
    ut <- get("usageTable", envir = .gridSVGEnv)
    nrow(ut) > 0 && any(ut$type == "ref")
}

# Methods used for grabbing the list of references used by a definition.
# Particularly useful in the case of patterns where it could contain
# content which also references other content. In other words, it allows
# us to be able to get a tree of dependencies, rather than just a flat
# list.
labelsUsed <- function(x) {
    UseMethod("labelsUsed")
}

labelsUsed.patternFillRefDef <- function(x) {
    NULL
}

labelsUsed.filterDef <- function(x) {
    NULL
}

labelsUsed.gradientDef <- function(x) {
    NULL
}

labelsUsed.patternFillDef <- function(x) {
    labelsUsed(x$grob)
}

labelsUsed.maskDef <- function(x) {
    labelsUsed(x$grob)
}

labelsUsed.clipPathDef <- function(x) {
    labelsUsed(x$grob)
}

labelsUsed.grob <- function(x) {
    x$referenceLabel
}

labelsUsed.gTree <- function(x) {
    unlist(lapply(x$children, labelsUsed))
}

# Used for knowing whether to write out a definition.
# If a definition has not been used we do not write it out.
# If it has been used more than once we do not repeat the
# definition.
isLabelUsed <- function(label) {
    rut <- get("refUsageTable", envir = .gridSVGEnv)
    rut[rut$label == label, "used"]
}

setLabelUsed <- function(label) {
    if (! is.null(label)) {
        rut <- get("refUsageTable", envir = .gridSVGEnv)
        rut[rut$label %in% label, "used"] <- TRUE
        assign("refUsageTable", rut, envir = .gridSVGEnv)
    }
}

# Convenience function to list all referenced content definitions
listSVGDefinitions <- function(print = TRUE) {
    refdefs <- get("refDefinitions", envir = .gridSVGEnv)
    n <- length(refdefs)
    if (!n)
        return(invisible())
    defs <- data.frame(label = character(n),
                       type = character(n),
                       refLabel = character(n),
                       stringsAsFactors = FALSE)

    for (i in 1:n) {
        curdef <- refdefs[[i]]
        defs$label[i] <- curdef$label
        if (! is.null(curdef$refLabel))
            defs$refLabel[i] <- curdef$refLabel
        defs$type[i] <- switch(class(curdef)[1],
                               clipPathDef = "Clipping Path",
                               filterDef = "Filter Effect",
                               gradientDef = "Gradient Fill",
                               maskDef = "Mask",
                               patternFillDef = "Pattern Fill",
                               patternFillRefDef = "Pattern Fill Reference",
                               "")
    }

    if (print) {
        orderedTypes <- sort(unique(defs$type))
        indent <- "  "
        cat("Reference Definitions\n")
        for (i in 1:length(orderedTypes)) {
            typesub <- defs[defs$type == orderedTypes[i], ]
            cat("\n", orderedTypes[i], "s\n", sep = "")
            for (j in 1:nrow(typesub)) {
                cat(indent, typesub$label[j], sep = "")
                # If this is a pattern fill, show us what we're referencing
                if (nchar(typesub$refLabel[j]))
                    cat(" ", paste0("(referencing ", typesub$refLabel[j], ")"),
                        "\n", sep = "")
                else
                    cat("\n")
            }
        }
    }

    invisible(defs)
}

checkForDefinition <- function(label) {
    if (! label %in% names(get("refDefinitions", envir = .gridSVGEnv)))
        stop("A reference definition must be created before using this label")
}

checkExistingDefinition <- function(label) {
    if (label %in% names(get("refDefinitions", envir = .gridSVGEnv)))
        stop(paste("label", sQuote(label),
                   "already exists as a reference definition")) 
}

# When we need to generate a temporary label (i.e. when specifying a
# gradient fill directly on a grob with no label), we supply a prefix and
# return a new label that is going to be unique (among labels).
# getID() will perform the task of ensuring uniqueness among IDs.
getNewLabel <- function(prefix) {
    i <- 1
    candidateName <- paste0(prefix, ".", i)
    refdefs <- get("refDefinitions", envir = .gridSVGEnv)
    while(candidateName %in% names(refdefs)) {
        i <- i + 1
        candidateName <- paste0(prefix, getSVGoption("id.sep"), i)
    }
    candidateName
}

getLabelID <- function(label) {
    ut <- get("usageTable", envir = .gridSVGEnv)
    suffix <- ut[ut$name == label & ut$type == "ref", "suffix"]
    prefixName(paste0(label, getSVGoption("id.sep"), suffix))
}

