grid.comment <- function(name, comment, vp = NULL) {
    grid.draw(commentGrob(name, comment, vp))
}

commentGrob <- function(name, comment, vp = NULL) {
    ng <- nullGrob(name = name, vp = vp)
    ng$comment <- comment
    cl <- class(ng)
    class(ng) <- unique(c("comment.grob", cl))
    ng
}

primToDev.comment.grob <- function(x, dev) {
    svgComment(x$comment, dev@dev)
}
