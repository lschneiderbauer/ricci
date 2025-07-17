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
#' Provides a labeled Levi-Civita epsilon (pseudo) tensor.
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
#' Provides a labeled Minkowski metric tensor in `n` dimensions in
#' Cartesian coordinates with signature \eqn{(-1, 1, 1, ...)}{`c(-1, 1, 1, ...)`}.
#' Note that the numeric result will be different depending on the
#' chosen index positions.
#' If no labels are specified, an unlabeled n by n
#' array of the covariant metric tensor is returned.
#'
#' @param n The dimension of the metric tensor.
#' @return
#'  A function that expects index labels (see [.()]) and returns a
#'  labeled tensor with dimension `c(n, n)`.
#'
#' @seealso Wikipedia [Minkowski metric tensor](https://en.wikipedia.org/wiki/Minkowski_space#Minkowski_metric)
#' @examples
#' g_mink(4)(i, j)
#' g_mink(4)(+i, +j)
#' g_mink(4)()
#' @export
#' @concept tensor_instance
#' @family metric tensors
g_mink <- function(n) {
  callf <- rlang::current_call()
  function(..., call = callf) {
    i <- .(...)

    validate_metric_tensor_indices(i, call)

    if (length(i$i) == 0) {
      return(diag(c(-1, rep(1, n - 1)), n, n))
    }

    if (setequal(i$p, "+") || setequal(i$p, "-")) {
      # in this case the inverse is equal to g,
      # so we don't need to calculate it
      g <- diag(c(-1, rep(1, n - 1)), n, n)

      tensor(g, i$i, i$p)
    }
  }
}


#' Euclidean metric tensor
#'
#' Provides a labeled Euclidean metric tensor of \eqn{\mathbb{E}^n}{E^n}.
#' Note that the numeric result will be different depending on the
#' chosen index positions.
#' If no labels are specified, an unlabeled n by n
#' array of the covariant metric tensor is returned.
#'
#' @details
#' `g_eucl_cart()` returns a numeric (constant) tensor in Cartesian coordinates,
#' while `g_eucl_sph()` returns a symbolic tensor field in generalized spherical
#' coordinates \eqn{{r, \phi_1, \phi_2, ..., \phi_{n-1}}}{{r, `ph1`, `ph2`, ..., `ph(n-2)`}}.
#' As usual, spherical coordinates are degenerate at \eqn{\phi_l = 0}, so be
#' careful around those points.
#'
#' @param n The dimension of the metric tensor.
#' @return
#'  A function that expects index labels (see [.()]) and returns a
#'  labeled array with dimension `c(n, n)`.
#'
#' @seealso Wikipedia: [Euclidean metric tensor](https://en.wikipedia.org/wiki/Metric_tensor#Euclidean_metric)
#' @examples
#' g_eucl_cart(3)(i, j)
#' g_eucl_cart(3)(+i, +j)
#' g_eucl_cart(3)()
#' @export
#' @concept tensor_instance
#' @family metric tensors
#' @rdname g_eucl
g_eucl_cart <- function(n) {
  callf <- rlang::current_call()
  function(..., call = callf) {
    i <- .(...)

    validate_metric_tensor_indices(i, call)

    if (length(i$i) == 0) {
      diag(1, n, n)
    } else {
      tensor(diag(1, n, n), i$i, i$p)
    }
  }
}

#' @export
#' @examples
#' g_eucl_sph(3)(i, j)
#' g_eucl_sph(3)(+i, +j)
#' g_eucl_sph(3)()
#' @rdname g_eucl
g_eucl_sph <- function(n) {
  callf <- rlang::current_call()
  function(..., call = callf) {
    i <- .(...)

    validate_metric_tensor_indices(i, call)

    # construct matrix
    mat_diag <-
      c(
        "1",
        vapply(
          1:(n - 1),
          function(k) {
            if (k - 1 < 1) {
              return("r^2")
            }

            calculus::`%prod%`(
              "r^2",
              Reduce(
                calculus::`%prod%`,
                vapply(
                  1:(k - 1),
                  function(m) {
                    paste0("sin(ph", m, ")^2")
                  },
                  FUN.VALUE = ""
                )
              )
            )
          },
          FUN.VALUE = ""
        )
      )

    if (length(i$p) > 0 && unique(i$p) == "+") {
      mat_diag <- calculus::`%div%`("1", mat_diag)
    }

    mat <- array("0", c(n, n))
    diag(mat) <- mat_diag

    if (length(i$i) == 0) {
      mat
    } else {
      tensor(mat, i$i, i$p)
    }
  }
}
