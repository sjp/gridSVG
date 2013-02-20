
# Get and set global 'gridSVG' options

# Initial settings
assign("gridSVGoptions",
       list(
           id.sep="."
           ),
       .gridSVGEnv)

checkOptions <- function(options) {
    if (!is.character(options[["id.sep"]]))
        stop("Invalid option")
}

# Get/set options
getSVGoption <- function(name) {
    oldOptions <- get("gridSVGoptions", .gridSVGEnv)
    optionNames <- names(oldOptions)
    if (name %in% optionNames) {
        oldOptions[[name]]
    }
}

getSVGoptions <- function() {
    get("gridSVGoptions", .gridSVGEnv)
}

setSVGoptions <- function(...) {
    oldOptions <- get("gridSVGoptions", .gridSVGEnv)
    options <- list(...)
    if (length(options)) {
        names <- names(options)
        optionNames <- names(oldOptions)
        names <- names[nchar(names) > 0 &
                       names %in% optionNames]
        if (length(options[names])) {
            newOptions <- oldOptions
            newOptions[names] <- options[names]
            checkOptions(newOptions)
            assign("gridSVGoptions", newOptions, .gridSVGEnv)
            invisible(oldOptions[names])
        } 
    } 
}
