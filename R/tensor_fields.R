#' Minkowski metric tensor
#'
#' `g_mink_cart()` provides the covariant metric tensor in `n` dimensions in
#' Cartesian coordinates with signature \eqn{(-1, 1, 1, ...)}{`c(-1, 1, 1, ...)`}.
#' `g_mink_sph()` provides the same tensor where the spatial part uses spherical
#' coordinates.
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
#' g_mink_cart(4)
#' g_mink_cart(4) %_% .(+i, +j)
#' @export
#' @concept metric_tensors
#' @family metric tensors
#' @rdname g_mink
g_mink_cart <- function(n, coords = paste0("x", 1:n - 1)) {
  metric_field(
    diag(c(-1, rep(1, n - 1)), n, n),
    diag(c(-1, rep(1, n - 1)), n, n),
    coords = paste0("x", 1:n - 1)
  )
}

#' @export
#' @examples
#' g_mink_sph(4)
#' g_mink_sph(4) %_% .(+i, +j)
#' @rdname g_mink
g_mink_sph <- function(n, coords = c("t", "r", paste0("ph", 1:(n - 2)))) {
  if (n == 1) {
    return(
      return(
        metric_field(
          array(-1, 1),
          array(-1, 1),
          coords = "t"
        )
      )
    )
  }

  g_sph <- g_eucl_sph(n - 1, coords = coords[-1])

  mat <- matrix(0, n, n)
  mat[2:n, 2:n] <- g_sph
  mat[1,1] <- "-1"

  mat_inv <- matrix(0, n, n)
  mat_inv[2:n, 2:n] <- metric_inv(g_sph)
  mat_inv[1,1] <- "-1"

  if (n == 2) {
    coords <- c("t", "r")
  }

  metric_field(
    mat, mat_inv, coords
  )
}


#' Euclidean metric tensor
#'
#' Provides the Euclidean metric tensor of \eqn{\mathbb{E}^n}{E^n}.
#'
#' @details
#' `g_eucl_cart()` returns a numeric (constant) tensor in Cartesian coordinates,
#' while `g_eucl_sph()` returns a symbolic tensor field in generalized spherical
#' coordinates \eqn{{r, \phi_1, \phi_2, ..., \phi_{n-1}}}{{r, `ph1`, `ph2`, ..., `ph(n-2)`}}.
#' As usual, spherical coordinates are degenerate at \eqn{r = 0} and \eqn{\phi_l = 0}, so be
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
#' @concept metric_tensors
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
  if (n == 1) {
    return(
      metric_field(
        array(1, 1),
        array(1, 1),
        coords = "r"
      )
    )
  }

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

#' Metric tensor of sphere
#'
#' Provides the metric tensor of the sphere \eqn{S^n} with radius 1.
#'
#' @details
#' `g_sph()` returns a symbolic tensor field in generalized spherical
#' coordinates \eqn{{\phi_1, \phi_2, ..., \phi_{n-1}}}{{r, `ph1`, `ph2`, ..., `ph(n-2)`}}.
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
#' @seealso Wikipedia: [Sphere](https://en.wikipedia.org/wiki/Sphere)
#' @examples
#' g_sph(3)
#' g_sph(3) %_% .(+i, +j)
#' @export
#' @concept metric_tensors
#' @family metric tensors
g_sph <- function(n, coords = paste0("ph", 1:n)) {
  # construct matrix
  mat_diag <-
    vapply(
      1:n,
      function(k) {
        if (k - 1 < 1) {
          return("1")
        }

        Reduce(
          calculus::`%prod%`,
          vapply(
            1:(k - 1),
            function(m) {
              paste0("sin(", coords[[m]], ")^2")
            },
            FUN.VALUE = ""
          )
        )
      },
      FUN.VALUE = ""
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

# used indices
globalVariables(c("i", "j", "k", "l", "i2", "s"))

#' Christoffel symbols
#'
#' Provides the Christoffel symbols of the first kind \eqn{\Gamma_{ijk}} with
#' respect to the Levi Civita connection for a given metric tensor.
#'
#' The Christoffel symbols are a rank 3 array of numbers.
#'
#' @param g
#'  A covariant metric tensor, a "metric_field" object. See [metric_field()]
#'  to create a new metric tensor, or use predefined metrics,
#'  e.g. [g_eucl_cart()].
#' @return
#'  Returns the Christoffel symbols of the first kind \eqn{\Gamma_{ijk}}
#'  as rank 3 [array()].
#'
#' @examples
#' christoffel(g_eucl_sph(3))
#' @seealso Wikipedia: [Christoffel symbols](https://en.wikipedia.org/wiki/Christoffel_symbols])
#' @export
#' @concept geom_tensors
#' @family geometric tensors
christoffel <- function(g) {
  stopifnot(inherits(g, "metric_field"))

  coords <- metric_coords(g)

  der <- calculus::derivative(g, coords)
  ((der %_% .(i, k, l) + der %_% .(i, l, k) - der %_% .(k, l, i)) / 2L) |>
    as_a(i, k, l)
}

#' Riemann curvature tensor
#'
#' Provides the covariant Riemann curvature tensor \eqn{R_{ijkl}}.
#'
#' @inheritParams christoffel
#' @return
#'  Returns the covariant Riemann curvature tensor \eqn{R_{ijkl}}
#'  as rank 4 [array()].
#'
#' @examples
#' riemann(g_eucl_sph(3))
#' @seealso Wikipedia: [Riemann curvature tensor](https://en.wikipedia.org/wiki/Riemann_curvature_tensor)
#' @export
#' @concept geom_tensors
#' @family geometric tensors
riemann <- function(g) {
  stopifnot(inherits(g, "metric_field"))

  coords <- metric_coords(g)
  chr <- (christoffel(g) %_% .(i, j, l) * g %_% .(+i, +k)) |> as_a(+k, j, l)

  ((pd(chr %_% .(+i, j, k), coords, "l", "-", g) +
    chr %_% .(+i, l, s) * chr %_% .(+s, j, k)) * g %_% .(i, i2)) |>
    asym(j, l) |>
    as_a(i2, k, j, l)
}

#' Ricci curvature tensor
#'
#' Provides the covariant Ricci curvature tensor \eqn{R_{ij}=R^{s}_{i s j}}.
#'
#' @inheritParams christoffel
#' @return
#'  Returns the covariant Ricci curvature tensor \eqn{R_{ij}}
#'  as rank 2 [array()].
#'
#' @examples
#' ricci(g_eucl_sph(3))
#' @seealso Wikipedia: [Ricci curvature tensor](https://en.wikipedia.org/wiki/Riemann_curvature_tensor#Ricci_curvature)
#' @export
#' @concept geom_tensors
#' @family geometric tensors
ricci <- function(g) {
  (riemann(g) %_% .(i, j, k, l) * g %_% .(+i, +k)) |>
    as_a(j, l)
}

#' Ricci scalar
#'
#' Provides the Ricci scalar \eqn{R}.
#'
#' @inheritParams christoffel
#' @return
#'  Returns the Ricci scalar \eqn{R} as single number/expression.
#'
#' @examples
#' ricci_sc(g_eucl_sph(3))
#' @seealso Wikipedia: [Ricci scalar](https://en.wikipedia.org/wiki/Scalar_curvature)
#' @export
#' @concept geom_tensors
#' @family geometric tensors
ricci_sc <- function(g) {
  (ricci(g) %_% .(i, j) * g %_% .(+i, +j)) |> as_a()
}
