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
