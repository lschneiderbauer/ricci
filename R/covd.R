# used indices
globalVariables(c("i", "j", "k", "l", "i2"))

#' Christoffel symbols
#'
#' Provides the Christoffel symbols of the first kind \eqn{Gamma_{ijk}} with
#' respect to the Levi Civita connection for a given metric tensor.
#'
#' The Christoffel symbols are a rank 3 array of numbers.
#'
#' @param g
#'  A covariant metric tensor, a "metric_field" object. See [metric_field()]
#'  to create a new metric tensor, or use predefined metrics,
#'  e.g. [g_eucl_cart()].
#' @return
#'  Returns the Christoffel symbols of the first kind \eqn{Gamma_{ijk}}
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
  ((der %_% .(i, k, l) + der %_% .(i, l, k) - der %_% .(k, l, i)) / 2) |>
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

  ((pd(chr %_% .(+i, j, k), coords, "l") +
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

# new_index_name: is a lower index
pd <- function(x, coords, new_index_name) {
  stopifnot(inherits(x, "tensor"))

  new_tensor(
    calculus::derivative(x, coords),
    index_names = c(tensor_index_names(x), new_index_name),
    index_positions = c(tensor_index_positions(x), FALSE)
  )
}

covd <- function(g) {
  chr <-
    christoffel(g) %_% .(i, j, k) |>
    r(i, g = g) |>
    as_a(+i, j, k)

  coords <- metric_coords(g)

  # ... : new index label
  function(x, ..., act_on = all()) {
    stopifnot(inherits(x, "tensor"))
    # x needs to be a labeled array / tensor (not an array)
    # because we need to know which indices are lowered and
    # which are upped

    new_ind <- .(...)

    # TODO: check that we got only one index

    partiald <- pd(x, coords, new_ind$i)

    # for each upper index one + chr term
    # for each lower index one - chr term
    pos <- tensor_index_positions(x)
    Reduce(
      function(index_name, tadd) {
        if (pos[index_name] == "+") {
          tadd +
            tensor(chr, c(index_name, "?", new_ind$i), c("+", "-", "-")) *
              x |> subst(+!!index_name -> +`?`)
        } else {
          tadd -
            tensor(chr, c("?", index_name, new_ind$i), c("+", "-", "-")) *
              x |> subst(!!index_name -> `?`)
        }
      },
      tensor_index_names(x),
      init = partiald
    )

    # TODO: raise index if required
  }
}
