#' Raise tensor indices
#'
#' `r()` raises specified tensor indices using a covariant metric tensor
#' provided in `g`.
#' Note that the order of indices is not preserved due to performance reasons.
#' An error is thrown if the specified indices do not exist or are not in the
#' correct position.
#'
#' @param x  A labeled tensor object, created by [`%_%`] or [tensor()].
#' @param g
#'  An (unlabeled) covariant metric tensor as array used for raising and
#'  lowering indices.
#'  If no metric tensor is provided, indices are raised/lowered with
#'  the identity matrix.
#' @param ... Any number of index expressions. The indices need to occur
#'            in `x`.
#' @return A modified tensor object.
#'
#' @export
#' @concept tensor_ops
#' @family tensor operations
r <- function(x, ..., g = NULL) {
  tensor_raise(x, .(...), g,
    arg_x = rlang::caller_arg(x),
    arg_ind = "...",
    arg_g = rlang::caller_arg(g)
  ) |>
    tensor_reduce()
}

#' Lower tensor indices
#'
#' `l()` lowers specified tensor indices using a covariant metric tensor
#' provided in `g`.
#' Note that the order of indices is not preserved due to performance reasons.
#' An error is thrown if the specified indices do not exist or are not in the
#' correct position.
#'
#' @inheritParams r
#' @return A modified tensor object.
#' @export
#' @concept tensor_ops
#' @family tensor operations
l <- function(x, ..., g = NULL) {
  tensor_lower(x, .(...), g,
    arg_x = rlang::caller_arg(x),
    arg_ind = "...",
    arg_g = rlang::caller_arg(g)
  ) |>
    tensor_reduce()
}

#' Substitute tensor labels
#'
#' Substitutes tensor labels with other labels. This is simply a renaming
#' procedure. The operation might trigger implicit diagonal subsetting.
#' An error is thrown if the specified indices do not exist.
#'
#' @param x  A labeled tensor object, created by [`%_%`] or [tensor()].
#' @param ... Any number of expressions (separated by a comma) of the form
#'            `<label1> -> <label2>`.
#' @return A modified tensor object.
#'
#' @export
#' @concept tensor_ops
#' @family tensor operations
subst <- function(x, ...) {
  tensor_indices <- Map(ast_subst, rlang::exprs(...))

  ind_from <-
    new_tensor_indices(
      i = unlist(Map(\(x) x$ind_from$i, tensor_indices)),
      p = unlist(Map(\(x) x$ind_from$p, tensor_indices))
    )

  ind_to <-
    new_tensor_indices(
      i = unlist(Map(\(x) x$ind_to$i, tensor_indices)),
      p = unlist(Map(\(x) x$ind_to$p, tensor_indices))
    )

  tensor_subst(x, ind_from, ind_to, arg = "...") |>
    tensor_reduce()
}

#' Symmetric tensor part
#'
#' Takes the symmetric tensor part of a tensor `x` with respect to the
#' specified indices `...`.
#' An error is thrown if the specified indices do not exist.
#'
#' @param x  A labeled tensor object, created by [`%_%`] or [tensor()].
#' @param ... Any number of index expressions. The indices need to occur
#'            in `x`.
#' @return A modified tensor object.
#' @examples
#' a <- array(1:4, dim = c(2, 2))
#' a %_% .(i, j) |> sym(i, j)
#' @export
#' @seealso Wikipedia: [Ricci calculus - Symmetric and antisymmetric parts](https://en.wikipedia.org/wiki/Ricci_calculus#Symmetric_and_antisymmetric_parts)
#' @concept tensor_ops
#' @family tensor operations
sym <- function(x, ...) {
  tensor_sym(x, .(...)) |>
    tensor_reduce()
}

#' Antisymmetric tensor part
#'
#' Takes the antisymmetric tensor part of a tensor `x` with respect to the
#' specified indices `...`.
#' An error is thrown if the specified indices do not exist.
#'
#' @param x  A labeled tensor object, created by [`%_%`] or [tensor()].
#' @param ... Any number of index expressions. The indices need to occur
#'            in `x`.
#' @return A modified tensor object.
#'
#' @examples
#' a <- array(1:4, dim = c(2, 2))
#' a %_% .(i, j) |> asym(i, j)
#' @export
#' @seealso Wikipedia: [Ricci calculus - Symmetric and antisymmetric parts](https://en.wikipedia.org/wiki/Ricci_calculus#Symmetric_and_antisymmetric_parts)
#' @concept tensor_ops
#' @family tensor operations
asym <- function(x, ...) {
  tensor_asym(x, .(...), arg = "...") |>
    tensor_reduce()
}

#' Kronecker product
#'
#' A Kronecker product is simply a tensor product whose underlying vector
#' space basis is relabeled. In the present context this is realized
#' by combining multiple index labels into one. The associated dimension
#' to the new label is then simply the product of the dimensions associated
#' to the old index labels respectively.
#'
#' @param x  A labeled tensor object, created by [`%_%`] or [tensor()].
#' @param ... Any number of expressions (separated by a comma) of the form
#'            `.(<label1>, <label2>, ..., <labeln-1>) -> <labeln+1>`.
#' @return A modified tensor object.
#'
#' @examples
#' a <- array(1:8, dim = c(2, 2, 2))
#' a %_% .(i, j, k) |> kron(.(i, j) -> l)
#' @export
#' @seealso Wikipedia: [Kronecker Product](https://en.wikipedia.org/wiki/Kronecker_product)
#' @concept tensor_ops
#' @family tensor operations
kron <- function(x, ...) {
  exprs <- rlang::exprs(...)

  Reduce(
    function(tens, expr) {
      l <- ast_kr(expr)

      tensor_kron(
        tens, l$ind_from, l$ind_to,
        arg = "...", call = rlang::env_parent()
      ) |>
        tensor_reduce()
    },
    exprs,
    init = x
  )
}

tensor_subst <- function(x, ind_from, ind_to,
                         arg = rlang::caller_arg(ind_from),
                         call = rlang::caller_env()) {
  tensor_validate_index_matching(
    x, ind_from,
    arg = arg, call = call
  )

  # index position must match between ind_from and ind_to
  validate_index_position(
    ind_to, ind_from$p,
    info = "Substitions need to leave index position invariant.",
    arg = arg, call = call
  )

  names <- tensor_index_names(x)
  names[match(ind_from$i, names)] <- ind_to$i

  new_tensor(
    x,
    names,
    tensor_index_positions(x)
  )
}

tensor_raise <- function(x, ind_from, g,
                         arg_x = rlang::caller_arg(x),
                         arg_ind = rlang::caller_arg(ind_from),
                         arg_g = rlang::caller_arg(g),
                         call = rlang::caller_env()) {
  tensor_validate_index_matching(
    x, ind_from,
    arg = arg_ind,
    call = call
  )

  # index must be lowered
  validate_index_position(
    ind_from, "-",
    info = "Only lowered indices can be raised.",
    arg = arg_ind,
    call = call
  )

  if (is.null(g)) {
    n <- tensor_dim(x, ind_from$i)
    g <- diag(1, n, n)
    ginv <- g
  } else {
    ginv <- metric_inv(g)
  }

  Reduce(
    function(tens, i) {
      tginv <- new_tensor(ginv, c(i, "?"), c(TRUE, TRUE))

      # check that g has the right dimensions
      tensor_validate_index_dim(x, tginv, arg_x, arg_g, call)

      tensor_subst(
        tens * tginv,
        new_tensor_indices(i = "?", p = "+"),
        new_tensor_indices(i = i, p = "+")
      )
    },
    ind_from$i,
    init = x
  )
}

tensor_lower <- function(x, ind_from, g,
                         arg_x = rlang::caller_arg(x),
                         arg_ind = rlang::caller_arg(ind_from),
                         arg_g = rlang::caller_arg(g),
                         call = rlang::caller_env()) {
  tensor_validate_index_matching(
    x, ind_from,
    arg = arg_ind,
    call = call
  )

  # index must be lowered
  validate_index_position(
    ind_from, "+",
    info = "Only raised indices can be lowered.",
    arg = arg_ind,
    call = call
  )

  if (is.null(g)) {
    n <- tensor_dim(x, ind_from$i)
    g <- diag(1, n, n)
  }

  if (is.function(g)) {
    g <- g()
  }

  Reduce(
    function(tens, i) {
      tg <- new_tensor(g, c(i, "?"), c(FALSE, FALSE))

      # check that g has the right dimensions
      tensor_validate_index_dim(x, tg, arg_x, arg_g, call)

      tensor_subst(
        tens * tg,
        new_tensor_indices(i = "?", p = "-"),
        new_tensor_indices(i = i, p = "-")
      )
    },
    ind_from$i,
    init = x
  )
}

# generalized kronecker product: combines two (or more) indices i,j to a
# single index k where dim(k) = dim(i)*dim(j)
#' @importFrom cli cli_abort
tensor_kron <- function(x, ind_comb, ind_new,
                        arg = rlang::caller_arg(ind_comb),
                        call = rlang::caller_env()) {
  tensor_validate_index_matching(
    x, ind_comb,
    arg = arg, call = call
  )

  # we require the combining indices to have the same position
  if (length(unique(ind_comb$p)) > 1) {
    cli_abort(
      c(
        "In {.arg {arg}}: indices with different positions cannot be combined.",
        x = "Indices {.code {ind_comb$i}} have different position{?s}.",
        i = "Make sure you combine only indices of the same position."
      ),
      call = call
    )
  }

  # index position must match between ind_comb and ind_new
  validate_index_position(
    ind_new, unique(ind_comb$p),
    info = "The Kronecker product can only create an index of the same position.",
    arg = arg, call = call
  )


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
    index_positions = c(ind_new$p == "+", tensor_index_positions(x)[ind_invariant])
  ) |>
    tensor_reduce()
}

#' @importFrom cli cli_warn
tensor_asym <- function(x, ind,
                        arg = rlang::caller_arg(ind),
                        call = rlang::caller_env()) {
  tensor_validate_index_matching(
    x, ind,
    arg = arg, call = call
  )

  if (length(ind$i) == 1) {
    cli_warn(
      c(
        "Antisymmetrization over a single index has no effect.",
        i = "You might want to consider to remove this call."
      ),
      call = call
    )
    return(x)
  }

  if (!tensor_is_reduced(x)) {
    x <- tensor_reduce(x)
  }

  # the symmetrized indices need to have the same range (dimensions)
  n <- unique(tensor_dim(x, ind$i))
  stopifnot(length(n) == 1)

  p <- length(ind$i)

  tensor_asym <-
    tensor(
      calculus::delta(n, p),
      index_names = c(ind$i, paste0("?", ind$i)),
      index_positions = c(pos_inv(ind$p), ind$p)
    ) * x / factorial(p)

  tensor_subst(
    tensor_asym,
    new_tensor_indices(
      i = paste0("?", ind$i),
      p = ind$p
    ),
    ind
  )
}

#' @importFrom cli cli_warn
tensor_sym <- function(x, ind,
                       arg = rlang::caller_arg(ind),
                       call = rlang::caller_env()) {
  tensor_validate_index_matching(
    x, ind,
    arg = arg, call = call
  )

  if (length(ind$i) == 1) {
    cli_warn(
      c(
        "Symmetrization over a single index has no effect.",
        i = "You might want to consider to remove this call."
      ),
      call = call
    )
    return(x)
  }

  perms <- apply(permn(ind$i), MARGIN = 1, identity, simplify = FALSE)
  norm <- length(perms)

  ind_to <- ind

  Reduce(
    function(tens, perm) {
      ind_to$i <- perm
      tens + tensor_subst(x, ind, ind_to)
    },
    perms[-1],
    init = x
  ) / norm
}

permn <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(res)
  }
}

pos_inv <- function(pos) {
  pos[pos == "+"] <- "++"
  pos[pos == "-"] <- "+"
  pos[pos == "++"] <- "-"
  pos
}
