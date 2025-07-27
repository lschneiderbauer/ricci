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
#' Calculates the (symbolic) covariant derivative
#' \eqn{\nabla_\rho a_{\mu_{1} \mu_{2} ...}^{\nu_{1}\nu_{2}...}} with respect
#' to the Levi Civita connection of any (symbolic) tensor field.
#' The result is a new tensor of one rank higher than
#' the original tensor rank.
#'
#' @details
#' Note that symbolic derivatives are not always completely trustworthy.
#' They usually ignore subtle issues like undefined expressions at certain
#' points. The example \eqn{\nabla_a \nabla^a r^{-1}} from below is telling:
#' The symbolic derivative
#' evaluates to zero identically, although strictly speaking the derivative
#' is not defined at \eqn{r = 0}.
#'
#' @param x
#'  A labeled tensor object, created by [`%_%`] or [tensor()]. `covd()` only
#'  handles symbolic derivatives, i.e. the tensor components are required to be
#'  [character()]-valued and consist of mathematical expressions in terms of
#'  coordinates identical to the coordinates used by `g`.
#' @param i
#'  An index slot label specification created with [.()]. The number of
#'  indices specify the number of covariant derivatives taken in the same
#'  order. Each covariant derivative adds one index each with the specified
#'  names. After the covariant derivatives are calculated, implicit contraction
#'  rules are applied (in case of reoccurring index label names).
#' @inheritParams christoffel
#' @param act_on
#'  An optional index slot label specification created with [.()] that
#'  specifies on which indices the covariant derivative should act on.
#'  This might
#'  be useful if not all tensor factors are elements of the tangent space of
#'  the underlying manifold. If not provided the covariant derivative acts on
#'  all indices. If no indices are selected explicitly (with [.()]),
#'  the covariant derivative acts like it would on a scalar.
#' @return
#'  The covariant derivative: a new labeled array with one or more additional
#'  indices (depending on `i`).
#'
#' @examples
#' # gradient of "sin(sqrt(x1^2+x2^2+x3^2))" in 3-dimensional euclidean space
#' covd("sin(x1)", .(k), g = g_eucl_cart(3))
#'
#' # laplace operator
#' covd("sin(x1)", .(-k, +k), g = g_eucl_cart(3))
#' covd("1/r", .(-k, +k), g = g_eucl_sph(3))
#'
#' @seealso Wikipedia: [Covariant Derivative](https://en.wikipedia.org/wiki/Covariant_derivative)
#' @export
covd <- function(x, i, act_on = NULL, g = NULL) {
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

  if (is.null(act_on)) {
    act_on <-
      new_tensor_indices(
        tensor_index_names(x),
        tensor_index_positions(x)
      )
  } else {
    tensor_validate_index_matching(
      x, act_on,
      arg = rlang::caller_arg(act_on)
    )
  }

  chr <-
    christoffel(g) %_% .(i, j, k) |>
    r(i, g = g) |>
    as_a(+i, j, k)

  coords <- metric_coords(g)

  # TODO: check that dimension matches

  Reduce(
    function(x, l) {
      new_ind_name <- i$i[[l]]
      new_ind_pos <- i$p[[l]]

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
                - tensor(chr, c(dummy, index_name, new_ind_dummy), c("+", "-", "-")) *
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
          act_on$i,
          init = partiald
        )

      # add the new tensor indices to act_on (so we act on it the
      # next iteration)
      act_on <<-
        new_tensor_indices(
          c(act_on$i, new_ind_name),
          c(act_on$p, new_ind_pos)
        )

      tensor_reduce(res)
    },
    seq_along(i$i),
    init = tensor_reduce(x)
  )
}
