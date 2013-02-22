testUniqueMappings <- function(x) {
    idNodes <- getNodeSet(x, "//*[@id]", c(svg = "http://www.w3.org/2000/svg"))
    ids <- sapply(idNodes, function(x) xmlGetAttr(x, "id"))
    length(ids) == length(unique(ids))
}

formatTypeMapping <- function(x, type) {
    objs <- x[x$type == type, c("name", "suffix")]
    if (! nrow(objs))
        return(NULL)
    objNames <- unique(objs$name)
    objList <- vector("list", length(objNames))
    names(objList) <- objNames
    for (i in 1:length(objNames))
        objList[[i]] <- objs[objs$name == objNames[i], "suffix"]
    objList
}

formatMappings <- function(x) {
    list(vps = formatTypeMapping(x, "vp"),
         grobs =  formatTypeMapping(x, "grob"),
         id.sep = getSVGoption("id.sep"))
}

exportMappings <- function(x) {
    x <- formatMappings(x)
    paste("var gridSVGMappings = ", toJSON(x), ";\n", sep = "")
}

gridSVGMappingsGen <- function() {
    mappings <- NULL
    function(newmappings = NULL) {
        if (is.null(newmappings)) {
            mappings
        } else {
            mappings <<- newmappings
        }
    }
}

gridSVGMappings <- gridSVGMappingsGen()

getSVGMappings <- function(name, type, index = NULL) {
    if (! type %in% c("vp", "grob"))
        stop("'type' must be one of 'vp' or 'grob'")
    # Because the list itself uses vp/grob, rewrite
    type <- paste(type, "s", sep = "")
    mappings <- gridSVGMappings()
    if (is.null(mappings))
        stop("gridSVGMappings() must be initialised")
    suffixes <- mappings[[type]][[name]]
    if (is.null(suffixes))
        stop("Name not found")

    if (is.null(index)) {
        # Return all suffixes
        paste(name, suffixes, sep = mappings$id.sep)
    } else {
        # Return the matching indices only
        paste(name, suffixes[index], sep = mappings$id.sep)
    }
}

readMappingsJS <- function(filename) {
  jsData <- readLines(filename)
  jsData <- gsub("var gridSVGMappings = ", "", jsData)
  jsonData <- gsub(";$", "", jsData)
  paste0(jsonData, collapse = "\n")
}
