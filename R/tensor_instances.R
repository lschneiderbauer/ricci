#' Kronecker delta
#'
#' Provides a labeled generalized Kronecker delta. In the special
#' case of two labels this represents simply the identity matrix.
#' Note that the first half of the tensor labels need to be lowered,
#' while the second half needs upper indices. Otherwise an error is thrown.
#'
#' @param n The dimension.
#' @return
#'  A function that expects index labels (see [.()]) and returns a
#'  labeled tensor. The underlying data will differs depending on
#'  the number of labels provided.
#'
#' @seealso Underlying implementation: [calculus::delta()]
#' @seealso Wikipedia: [Generalized Kronecker delta](https://en.wikipedia.org/wiki/Kronecker_delta#generalized_Kronecker_delta)
#' @examples
#' d(3)(i, +j)
#'
#' d(3)(i, j, +k, +l)
#' @export
#' @concept tensor_instance
#' @family tensor symbols
d <- function(n) {
  function(...) {
    i <- .(...)
    order <- length(i$i)

    stopifnot(order %% 2 == 0)
    p <- order %/% 2

    # we require the first half of the indices to be lower,
    # the other half to be upper
    stopifnot(all(i$p[1:p] == "-"))
    stopifnot(all(i$p[p + 1:p] == "+"))

    tensor(
      calculus::delta(n = n, p = order %/% 2),
      i$i,
      i$p
    )
  }
}

#' Levi-Civita epsilon
#'
#' Provides a labeled Levi-Civita epsilon (pseude) tensor.
#' The indices are required to be lowered. Otherwise an error is thrown.
#'
#' @inheritParams .
#' @return
#'  A labeled tensor object.
#'  The underlying data will differs depending on
#'  the number of labels provided.
#'
#' @seealso Underlying implementation: [calculus::epsilon()]
#' @examples
#' e(i, j)
#'
#' e(i, j, k)
#' @seealso Wikipedia: [Levi-Civita symbol](https://en.wikipedia.org/wiki/Levi-Civita_symbol)
#' @export
#' @concept tensor_instance
#' @family tensor symbols
e <- function(...) {
  i <- .(...)

  stopifnot(all(i$p == "-"))

  tensor(
    calculus::epsilon(length(i$i)),
    i$i,
    i$p
  )
}

#' Minkowski metric tensor
#'
#' Provides a labeled Minkowski metric tensor in `n` dimensions.
#' Note that the numeric result will be different depending on the
#' chosen index positions.
#' If the labels are left empty, and unlabeled n by n
#' array of the covariant metric tensor is returned.
#'
#' @param dim The dimension of the metric tensor.
#' @return
#'  A function that expects index labels (see [.()]) and returns a
#'  labeled tensor with dimension `c(dim, dim)`.
#'
#' @examples
#' g_mink(4)(i, j)
#'
#' g_mink(4)(+i, +j)
#'
#' g_mink(4)()
#' @export
#' @concept tensor_instance
#' @family metric tensors
g_mink <- function(dim) {
  function(...) {
    i <- .(...)

    if (length(i$i) == 0) {
      return(diag(c(-1, rep(1, dim - 1)), dim, dim))
    }

    if (setequal(i$p, "+") || setequal(i$p, "-")) {
      # in this case the inverse is equal to g,
      # so we don't need to calculate it
      g <- diag(c(-1, rep(1, dim - 1)), dim, dim)

      tensor(g, i$i, i$p)
    } else {
      d(dim)(...)
    }
  }
}


#' Euclidean metric tensor
#'
#' Provides a labeled Euclidean metric tensor in `n` dimensions.
#' Note that the numeric result will be different depending on the
#' chosen index positions.
#' If the labels are left empty, an unlabeled n by n
#' array of the covariant metric tensor is returned.
#'
#' @param dim The dimension of the metric tensor.
#' @return
#'  A function that expects index labels (see [.()]) and returns a
#'  labeled tensor with dimension `c(dim, dim)`.
#'
#' @examples
#' g_eucl(4)(i, j)
#'
#' g_eucl(4)(+i, +j)
#'
#' g_eucl(4)()
#' @export
#' @concept tensor_instance
#' @family metric tensors
g_eucl <- function(dim) {
  function(...) {
    i <- .(...)

    if (length(i$i) == 0) {
      diag(1, dim, dim)
    } else {
      tensor(diag(1, dim, dim), i$i, i$p)
    }
  }
}
