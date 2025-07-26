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

# new_index_name: is a lower index

#' @importFrom utils tail
pd <- function(x, coords, new_index_name, new_index_position, g) {
  stopifnot(inherits(x, "tensor"))

  der <- calculus::derivative(x, coords)

  # the special case of a scalar is a bit annoying, because internally
  # its an array of dimension 1 and length 1.
  # i.e. the array after taking the derivative has dim c(1, n), where it
  # simply should have dim c(n)
  if (is_scalar(x)) {
    dim(der) <- tail(dim(der), n = 1)
  }

  dummy <- tensor_new_dummy_index_name(x)

  low <-
    new_tensor(
      der,
      index_names = c(tensor_index_names(x), dummy),
      index_positions = c(tensor_index_positions(x), FALSE)
    )

  res <-
    if (new_index_position == "+") {
      tensor_raise(
        low,
        ind_from = new_tensor_indices(dummy, "-"),
        g = g
      )
    } else {
      low
    }

  tensor_subst(
    res,
    new_tensor_indices(dummy, new_index_position),
    new_tensor_indices(new_index_name, new_index_position)
  )
}

#' Covariant Derivative
#'
#' Calculates the covariant derivative
#' \eqn{\nabla_\rho a_{\mu_{1} \mu_{2} ...}^{\nu_{1}\nu_{2}...}} with respect
#' to the Levi Civita connection of any tensor field.
#' The covariant derivative is a generalization of the directional derivative
#' that is independent of the employed coordinate system.
#' The result is a new tensor of one rank higher than
#' the original tensor rank.
#'
#' @seealso Wikipedia: [Covariant Derivative](https://en.wikipedia.org/wiki/Covariant_derivative)
#' @export
covd <- function(x, new_ind, g = NULL, act_on = all()) {
  if (is_scalar(x)) {
    # allow numbers as well
    x <- tensor(x)
  }
  stopifnot(inherits(x, "tensor"))
  # x needs to be a labeled array / tensor (not an array)
  # because we need to know which indices are lowered and
  # which are upped

  if (is.null(g)) {
    # TODO get rid of: assume homogeneous dimensions
    n <- unique(dim(x))
    g <- g_eucl_cart(n)
  }

  chr <-
    christoffel(g) %_% .(i, j, k) |>
    r(i, g = g) |>
    as_a(+i, j, k)

  coords <- metric_coords(g)

  # TODO: check that dimension matches

  Reduce(
    function(x, l) {
      new_ind_name <- new_ind$i[[l]]
      new_ind_pos <- new_ind$p[[l]]

      partiald <- pd(x, coords, new_ind_name, new_ind_pos, g)

      # for each upper index one + chr term
      # for each lower index one - chr term
      pos <- tensor_index_positions(x)

      dummy <- tensor_new_dummy_index_name(x)
      new_ind_dummy <- paste0(dummy, "2")

      res <-
        Reduce(
          function(tadd, index_name) {
            chrc <-
              if (pos[[index_name]] == TRUE) {
                tensor(chr, c(index_name, dummy, new_ind_dummy), c("+", "-", "-")) *
                  tensor_subst(
                    x,
                    new_tensor_indices(i = index_name, p = "+"),
                    new_tensor_indices(i = dummy, p = "+")
                  )
              } else {
                (-1) * tensor(chr, c(dummy, index_name, new_ind_dummy), c("+", "-", "-")) *
                  tensor_subst(
                    x,
                    new_tensor_indices(i = index_name, p = "-"),
                    new_tensor_indices(i = dummy, p = "-")
                  )
              }

            if (new_ind_pos == "+") {
              chrc <- chrc |> r(!!new_ind_dummy, g = g)
            }

            tadd +
              tensor_subst(
                chrc,
                new_tensor_indices(new_ind_dummy, new_ind_pos),
                new_tensor_indices(new_ind_name, new_ind_pos)
              )
          },
          tensor_index_names(x),
          init = partiald
        )

      tensor_reduce(res)
    },
    seq_along(new_ind$i),
    init = x
  )
}
