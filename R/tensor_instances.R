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
#'
#' @param n The dimension of the metric tensor.
#' @param coords
#'  A character vector of coordinate names. The length needs
#'  to match the tensor dimensions.
#' @return
#'  The covariant metric tensor as array imputed with coordinate names.
#'
#' @seealso Wikipedia [Minkowski metric tensor](https://en.wikipedia.org/wiki/Minkowski_space#Minkowski_metric)
#' @examples
#' g_mink(4)
#' g_mink(4) %_% .(+i, +j)
#' @export
#' @concept tensor_instance
#' @family metric tensors
g_mink <- function(n, coords = paste0("x", 1:n - 1)) {
  metric_field(
    diag(c(-1, rep(1, n - 1)), n, n),
    diag(c(-1, rep(1, n - 1)), n, n),
    coords = paste0("x", 1:n - 1)
  )
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
#' @param coords
#'  A character vector of coordinate names. The length needs
#'  to match the tensor dimensions.
#' @return
#'  The covariant metric tensor as array imputed with coordinate names.
#'
#' @seealso Wikipedia: [Euclidean metric tensor](https://en.wikipedia.org/wiki/Metric_tensor#Euclidean_metric)
#' @examples
#' g_eucl_cart(3)
#' g_eucl_cart(3) %_% .(+i, +j)
#' @export
#' @concept tensor_instance
#' @family metric tensors
#' @rdname g_eucl
g_eucl_cart <- function(n, coords = paste0("x", 1:n)) {
  metric_field(
    diag(1, n, n),
    diag(1, n, n),
    coords = coords
  )
}

#' @export
#' @examples
#' g_eucl_sph(3)
#' g_eucl_sph(3) %_% .(+i, +j)
#' @rdname g_eucl
g_eucl_sph <- function(n, coords = c("r", paste0("ph", 1:(n - 1)))) {
  # construct matrix
  mat_diag <-
    c(
      "1",
      vapply(
        1:(n - 1),
        function(k) {
          if (k - 1 < 1) {
            return(paste0(coords[[1]], "^2"))
          }

          calculus::`%prod%`(
            paste0(coords[[1]], "^2"),
            Reduce(
              calculus::`%prod%`,
              vapply(
                1:(k - 1),
                function(m) {
                  paste0("sin(", coords[[m + 1]], ")^2")
                },
                FUN.VALUE = ""
              )
            )
          )
        },
        FUN.VALUE = ""
      )
    )

  mat <- array("0", c(n, n))
  diag(mat) <- mat_diag

  mat_inv <- mat
  diag(mat_inv) <- calculus::`%div%`("1", diag(mat_inv))

  metric_field(
    mat,
    mat_inv,
    coords = coords
  )
}
