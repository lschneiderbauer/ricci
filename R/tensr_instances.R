#' Kronecker delta
#'
#' Provides a labeled generalized Kronecker delta.
#'
#' @param n The dimension.
#' @return A function that expects index labels. See [.()].
#'
#' @seealso [calculus::delta()]
#' @examples
#' d(3)(i, j)
#'
#' d(3)(i, j, k, l)
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

#' Levi Civita epsilon
#'
#' Provides a labeled Levi Civita epsilon tensor.
#'
#' @inheritParams .
#'
#' @seealso [calculus::epsilon()]
#' @examples
#' e(i, j)
#'
#' e(i, j, k)
#' @export
e <- function(...) {
  i <- .(...)

  tensr(
    calculus::epsilon(length(i$i)),
    i$i,
    i$p
  )
}

#' Minkowski metric tensor
#'
#' Provides a labeled Minkowski metric tensor in `n` dimensions.
#'
#' @param n The dimension.
#' @return A function that expects index labels. See [.()].
#'
#' @examples
#' g_mink(4)(i, j)
#'
#' g_mink(4)(+i, +j)
#' @export
g_mink <- function(n) {
  function(...) {
    i <- .(...)

    if (setequal(i$p, "+") || setequal(i$p, "-")) {
      # in this case the inverse is equal to g,
      # so we don't need to calculate it
      g <- diag(c(-1, rep(1, n - 1)), n, n)
      tensr(g, i$i, i$p)
    } else {
      d(n)(...)
    }
  }
}

#' Euclidean metric tensor
#'
#' Provides a labeled Euclidean metric tensor in `n` dimensions.
#'
#' @param n The dimension.
#' @return A function that expects index labels. See [.()].
#'
#' @examples
#' g_eucl(4)(i, j)
#'
#' g_eucl(4)(+i, +j)
#' @export
g_eucl <- function(n) {
  function(...) {
    i <- .(...)

    tensr(diag(1, n, n), i$i, i$p)
  }
}
