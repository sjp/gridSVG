grid.comment <- function(comment, name = NULL, vp = NULL) {
    grid.draw(commentGrob(comment, name, vp))
}

commentGrob <- function(comment, name = NULL, vp = NULL) {
    ng <- nullGrob(name = name, vp = vp)
    ng$comment <- comment
    cl <- class(ng)
    class(ng) <- unique(c("comment.grob", cl))
    ng
}

primToDev.comment.grob <- function(x, dev) {
    svgComment(x$comment, dev@dev)
}
