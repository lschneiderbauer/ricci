#' Index label related transformations
#'
#' `.t()` allows to substitute index labels, raising and lowering indices
#' and perform Kronecker products in any number or order. Multiple transformations
#' are separated by a comma ",".
#'
#' * Substituting index label (without altering position)
#' `<label1> -> <label2>`
#'
#' * Raising index:
#' `-<label1> -> +<label1>`
#'
#' * Lowering index:
#' `+<label1> -> -<label2>`
#'
#' * Kronecker product:
#' `.(<label1>, <label2>, ..., <labeln-1>) -> <labeln+1>`
#'
#' @param x  A labeled tensor object, created by [`%_%`] or [tensor()].
#' @param ... Any number of transformation specifications separated by a comma.
#' @param g
#'  An (unlabeled) covariant metric tensor as array used for raising and
#'  lowering indices.
#'  If no metric tensor is provided, indices are raised/lowered with
#'  the identity matrix.
#'
#' @return A modified tensor object.
#' @examples
#' a <- array(1:8, dim = c(2, 2, 2))
#'
#' # raising an index and contracting
#' a %_% .(i, j, k) |>
#'   .t(-j -> +i)
#'
#' # taking the Kronecker product
#' a %_% .(i, j, k) |>
#'   .t(.(j, k) -> l)
#' @export
#' @concept tensor
.t <- function(x, ..., g = NULL) {
  stopifnot(inherits(x, "tensor"))

  exprs <- rlang::exprs(...)

  Reduce(
    function(tens, expr) {
      l <- ast_extr_transf(expr)

      # we still need to check if the request makes sense
      # check that "from" indices actually exist in `x`
      # indices must exist
      stopifnot(l$ind_from$i %in% tensor_index_names(tens))
      # index positions must match
      stopifnot(
        all((l$ind_from$p == "+") == tensor_index_positions(tens)[l$ind_from$i])
      )

      switch(
        l$name,
        kron = tensor_kron(tens, l$ind_from, l$ind_to),
        raise = tensor_raise(tens, l$ind_from, l$ind_to, g),
        lower = tensor_lower(tens, l$ind_from, l$ind_to, g),
        subst = tensor_subst(tens, l$ind_from, l$ind_to)
      ) |>
        tensor_reduce()
    },
    exprs,
    init = x
  )
}

tensor_subst <- function(x, ind_from, ind_to) {
  names <- tensor_index_names(x)
  names[names == ind_from$i] <- ind_to$i

  new_tensor(
    x,
    names,
    tensor_index_positions(x)
  )
}

tensor_raise <- function(x, ind_from, ind_to, g) {
  if (is.null(g)) {
    n <- tensor_dim(x, ind_from$i)
    g <- diag(1, n, n)
  }

  if (is.function(g)) {
    g <- g()
  }

  tensor_subst(
    x * new_tensor(solve(g), c(ind_to$i, "?"), c(TRUE, TRUE)),
    list(i="?", p = "+"), ind_to
  )
}

tensor_lower <- function(x, ind_from, ind_to, g) {
  if (is.null(g)) {
    n <- tensor_dim(x, ind_from$i)
    g <- diag(1, n, n)
  }

  if (is.function(g)) {
    g <- g()
  }

  tensor_subst(
    x * new_tensor(g, c(ind_to$i, "?"), c(FALSE, FALSE)),
    list(i="?", p = "-"), ind_to
  )
}

# generalized kronecker product: combines two (or more) indices i,j to a
# single index k where dim(k) = dim(i)*dim(j)
tensor_kron <- function(x, ind_comb, ind_new) {
  # indices must exist
  stopifnot(ind_comb$i %in% tensor_index_names(x))
  # index positions must match
  stopifnot(all((ind_comb$p == "+") == tensor_index_positions(x)[ind_comb$i]))
  # we require the combining indices to have the same position
  ind_new_pos <- unique(tensor_index_positions(x)[ind_comb$i])
  stopifnot(length(ind_new_pos) == 1)
  stopifnot((ind_new$p == "+") == ind_new_pos)

  if (!tensor_is_reduced(x)) {
    x <- tensor_reduce(x)
  }

  # R has column major ordering
  # for instance tensor or rank 3 with dim = c(d1, d2, d3)
  # vec_addr = i + (j - 1) * d1 + (k - 1) * d2 * d1.
  #   i \in {1 .. d1}
  #   j \in {1 .. d2}
  #   k \in {1 .. d3}
  #
  # we can use this property to naturally produce a Kronecker product,
  # i.e. in this example,
  # set dim = c(d4, d3) with d4 = d1*d2, then
  # vec_addr = l + (k - 1) * d4
  #   l \in {1 .. d4 = d1 * d2}
  #   k \in {1 .. d3}
  #
  # so i + (j-1) d1 + (k-1) d2 d1 = l + (k-1) d1 d2
  #   => i + (j-1) d1 = l
  #
  # and we are mapping l to (i,j) without disturbing the other indices

  ind_invariant <- setdiff(tensor_index_names(x), ind_comb$i)

  # move combining indices to the beginning
  x <- tensor_reorder(x, c(ind_comb$i, ind_invariant))

  # reduce dimensions
  dim(x) <-
    c(
      prod(dim(x)[seq_along(ind_comb$i)]),
      dim(x)[-seq_along(ind_comb$i)]
    )

  new_tensor(
    x,
    index_names = c(ind_new$i, ind_invariant),
    index_positions = c(ind_new_pos, tensor_index_positions(x)[ind_invariant])
  ) |>
    tensor_reduce()
}

ast_extr_transf <- function(x) {
  if (rlang::is_call(x, "<-") && rlang::is_call(x[[3]], ".")) {
    ind_new <- .(!!x[[2]])
    ind_comb <- eval(x[[3]])

    if (length(ind_new$i) > 1) {
      stop("blah: only one index allowed")
    }

    return(
      list(
        name = "kron",
        ind_from = ind_comb,
        ind_to = ind_new
      )
    )
  } else if (rlang::is_call(x, "<-")) {
    ind_from <- .(!!x[[3]])
    ind_to <- .(!!x[[2]])

    if (ind_from$p != ind_to$p && ind_from$p == "+") {
      return(
        list(
          name = "lower",
          ind_from = ind_from,
          ind_to = ind_to
        )
      )
    } else if (ind_from$p != ind_to$p && ind_from$p == "-"){
      return(
        list(
          name = "raise",
          ind_from = ind_from,
          ind_to = ind_to
        )
      )
    } else if (ind_from$p == ind_to$p){
      return(
        list(
          name = "subst",
          ind_from = ind_from,
          ind_to = ind_to
        )
      )
    }

  } else {
    stop("blah")
  }

}
