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

#' @export
g_mink <- function(n) {
  function(...) {
    i <- .(...)

    if (setequal(i$p, "+") || setequal(i$p, "-")) {
      # in this case the inverse is equal to g
      g <- diag(c(-1, rep(1, n - 1)), n, n)
    } else {
      g <- d(n)(...)
    }

    tensr(g, i$i, i$p)
  }
}

#' @export
g_eukl <- function(n) {
  function(...) {
    i <- .(...)

    tensr(diag(1, n, n), i$i, i$p)
  }
}
