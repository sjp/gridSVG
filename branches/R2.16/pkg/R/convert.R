viewportCreate <- function(vpname, newname = NULL) {
    coords <- gridSVGCoords()
    if (is.null(coords))
        stop("gridSVGCoords() must be initialised")
    rootvp <- coords$ROOT
    if (is.null(rootvp))
        stop("the ROOT viewport must have coords info set")
    targetvp <- coords[[vpname]]
    if (is.null(vpname))
        stop(paste("the viewport", sQuote(vpname), "must have coords info set, see", sQuote("gridSVGCoords")))
    # Avoid having a vpPath as a viewport name
    if (is.null(newname)) {
        splitname <- strsplit(vpname, "::")[[1]]
        vpname <- tail(splitname, 1)
    } else {
        vpname <- newname
    }

    npcx <- targetvp$x / rootvp$width
    npcy <- targetvp$y / rootvp$height
    npcw <- targetvp$width / rootvp$width
    npch <- targetvp$height / rootvp$height
    viewport(x = unit(npcx, "npc"), y = unit(npcy, "npc"),
             width = unit(npcw, "npc"), height = unit(npch, "npc"),
             just = c("left", "bottom"), name = vpname,
             xscale = targetvp$xscale, yscale = targetvp$yscale)
}

viewportConvertX <- function(vpname, x, from, to = "svg") {
  currCoords <- validCoordsInfo(vpname)
  offset <- viewportConvertWidth(vpname, currCoords[[vpname]]$x,
                                 "svg", to)
  width <- viewportConvertWidth(vpname, x, from, to)
  offset + width
}

viewportConvertY <- function(vpname, x, from, to = "svg") {
  currCoords <- validCoordsInfo(vpname)
  offset <- viewportConvertHeight(vpname, currCoords[[vpname]]$y,
                                  "svg", to)
  height <- viewportConvertHeight(vpname, x, from, to)
  offset + height
}

viewportConvertWidth <- function(vpname, x, from, to) {
  currCoords <- validCoordsInfo(vpname)
  vpCoords <- currCoords[[vpname]]
  i <- toInches(from, x,
                vpCoords$width,
                vpCoords$xscale,
                vpCoords$inch)
  u <- toUnit(to, i,
              vpCoords$width,
              vpCoords$xscale,
              vpCoords$inch)
  round(u, 2)
}

viewportConvertHeight <- function(vpname, x, from, to) {
  currCoords <- validCoordsInfo(vpname)
  vpCoords <- currCoords[[vpname]]
  i <- toInches(from, x,
                vpCoords$height,
                vpCoords$yscale,
                vpCoords$inch)
  u <- toUnit(to, i,
              vpCoords$height,
              vpCoords$yscale,
              vpCoords$inch)
  round(u, 2)
}

toInches <- function(from, unitValue, vpDimSize, nativeScale, dimInchSize) {
  if (from == "inches")
    return(unitValue)

  nativeToInches <- function(nativeValue, nativeScale, vpDimSize, dimInchSize) {
    dist <- nativeValue - nativeScale[1]
    nativeUnitSize <- vpDimSize / abs(nativeScale[2] - nativeScale[1])
    dist * nativeUnitSize / dimInchSize
  }
  
  npcToInches <- function(npcValue, vpDimSize, dimInchSize) {
    (npcValue * vpDimSize) / dimInchSize
  }

  if (from == "native") {
    result <- nativeToInches(unitValue, nativeScale, vpDimSize, dimInchSize)
  } else if (from == "npc") {
     result <- npcToInches(unitValue, vpDimSize, dimInchSize)
  } else if (from == "svg") {
     result <- unitValue / dimInchSize
  } else {
    result <- convertUnit(unit(unitValue, from), "inches", valueOnly = TRUE)
  }

  result
}

toUnit <- function(to, unitValue, vpDimSize, nativeScale, dimInchSize) {
  if (to == "inches")
    return(unitValue)

  inchesToNative <- function(inchesValue, nativeScale, vpDimSize, dimInchSize) {
    npc <- (inchesValue * dimInchSize) / vpDimSize
    vpRange <- nativeScale[2] - nativeScale[1]
    (npc * vpRange) + nativeScale[1]
  }
  
  inchesToNpc <- function(inchesValue, vpDimSize, dimInchSize) {
    (inchesValue * dimInchSize) / vpDimSize
  }

  if (to == "native") {
    result <- inchesToNative(unitValue, nativeScale, vpDimSize, dimInchSize)
  } else if (to == "npc") {
    result <- inchesToNpc(unitValue, vpDimSize, dimInchSize)
  } else if (to == "svg") {
    result <- unitValue * dimInchSize
  } else {
    result <- convertUnit(unit(unitValue, "inches"), to, valueOnly = TRUE)
  }

  result
}

