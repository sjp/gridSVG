# Here are some fixes so that ggplot2 plots work properly! ggplot2 imports
# gtable and includes some nastiness that make it difficult for gridSVG to
# parse. To support the ggplot2 package, these compatibility features have
# been created.

grobToDev.gTableChild <- function(x, dev) {
  depth <- enforceVP(x$wrapvp, dev)
  NextMethod()
  unwindVP(x$wrapvp, depth, dev)
}


grobToDev.gTableParent <- function(x, dev) {
  depth <- enforceVP(x$layoutvp, dev)
  primToDev(x, dev)
  unwindVP(x$layoutvp, depth, dev)
}

# Ripped from gtable package's grid.draw.gtable method in grid.r.
# Note that the class ordering on a "gTableChild" is switched.
gTableGrob <- function(x) {
  if (length(x$grobs) == 0) return(invisible())

  children_vps <- mapply(child_vp,
    vp_name = vpname(x$layout),
    t = x$layout$t, r = x$layout$r, b = x$layout$b, l = x$layout$l,
    clip = x$layout$clip,
    SIMPLIFY = FALSE)

  x$grobs <- mapply(wrap_gtableChild, x$grobs, children_vps,
    SIMPLIFY = FALSE)

  if (inherits(x, "gTableChild")) {
    gt <- gTree(children = do.call("gList", x$grobs[order(x$layout$z)]),
      cl = c("gTableChild", "gTableParent"),
      vp = x$vp,
      wrapvp = x$wrapvp,
      layoutvp = viewport(layout = gtable_layout(x), name = x$name))
  } else {
    gt <- gTree(children = do.call("gList", x$grobs[order(x$layout$z)]),
      cl = "gTableParent",
      vp = x$vp,
      layoutvp = viewport(layout = gtable_layout(x), name = x$name))
  }

  gt
}

grobToDev.gtable <- function(x, dev) {
  grobToDev(gTableGrob(x), dev)
}


# Functions borrowed from 'gtable' to keep the package checker happy.
gtable_layout <- function(x) {
  # Commenting out because we won't be here if it's not a gtable
  # stopifnot(is.gtable(x))

  grid.layout(
    nrow = nrow(x), heights = x$heights,
    ncol = ncol(x), widths = x$widths,
    respect = x$respect
  )
}

vpname <- function(row) {
  paste(row$name, ".", row$t, "-", row$r, "-", row$b, "-", row$l, sep = "")
}

child_vp <- function(vp_name, t, r, b, l, clip) {
  viewport(name = vp_name, layout.pos.row = t:b,
           layout.pos.col = l:r, clip = clip)
}

wrap_gtableChild <- function(grob, vp) {
  grob$wrapvp <- vp
  grob$name <- vp$name
  class(grob) <- c("gTableChild", class(grob))
  grob
}
