#' @export
d <- function(n) {
  function(...) {
    i <- .(...)
    order <- length(i$i)

    stopifnot(order %% 2 == 0)

    tensr(
      calculus::delta(n = n, p = order %/% 2),
      i$i,
      i$p
    )
  }
}

#' @export
e <- function(...) {
  i <- .(...)

  tensr(
    calculus::epsilon(length(i$i)),
    i$i,
    i$p
  )
}
