#' Raise tensor indices
#'
#' `r()` raises specified tensor indices using a covariant metric tensor
#' provided in `g`.
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
  exprs <- rlang::exprs(...)

  Reduce(
    function(tens, expr) {
      l <- .(!!expr)

      # we still need to check if the request makes sense
      # check that "from" indices actually exist in `x`
      # indices must exist
      stopifnot(l$i %in% tensor_index_names(tens))
      # index positions must match
      stopifnot(
        all((l$p == "+") == tensor_index_positions(tens)[l$i])
      )
      # index must be lowered
      stopifnot(all(l$p == "-"))

      tensor_raise(tens, l, g) |>
        tensor_reduce()
    },
    exprs,
    init = x
  )
}

#' Lower tensor indices
#'
#' `l()` lowers specified tensor indices using a covariant metric tensor
#' provided in `g`.
#' An error is thrown if the specified indices do not exist or are not in the
#' correct position.
#'
#' @inheritParams r
#' @return A modified tensor object.
#' @export
#' @concept tensor_ops
#' @family tensor operations
l <- function(x, ..., g = NULL) {
  exprs <- rlang::exprs(...)

  Reduce(
    function(tens, expr) {
      l <- .(!!expr)

      # we still need to check if the request makes sense
      # check that "from" indices actually exist in `x`
      # indices must exist
      stopifnot(l$i %in% tensor_index_names(tens))
      # index positions must match
      stopifnot(
        all((l$p == "+") == tensor_index_positions(tens)[l$i])
      )
      # index must be raised
      stopifnot(all(l$p == "+"))

      tensor_lower(tens, l, g) |>
        tensor_reduce()
    },
    exprs,
    init = x
  )
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
  exprs <- rlang::exprs(...)

  Reduce(
    function(tens, expr) {
      l <- ast_subst(expr)

      # we still need to check if the request makes sense
      # check that "from" indices actually exist in `x`
      # indices must exist
      stopifnot(l$ind_from$i %in% tensor_index_names(tens))
      # index positions must match
      stopifnot(
        all((l$ind_from$p == "+") == tensor_index_positions(tens)[l$ind_from$i])
      )
      # index position must match between in and out
      stopifnot(all(l$ind_from$p == l$ind_to$p))

      tensor_subst(tens, l$ind_from, l$ind_to) |>
        tensor_reduce()
    },
    exprs,
    init = x
  )
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
  ind <- .(...)

  # we still need to check if the request makes sense
  # check that "from" indices actually exist in `x`
  # indices must exist
  stopifnot(ind$i %in% tensor_index_names(x))
  # index positions must match
  stopifnot(
    all((ind$p == "+") == tensor_index_positions(x)[ind$i])
  )

  tensor_sym(x, ind) |>
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
  ind <- .(...)

  # we still need to check if the request makes sense
  # check that "from" indices actually exist in `x`
  # indices must exist
  stopifnot(ind$i %in% tensor_index_names(x))
  # index positions must match
  stopifnot(
    all((ind$p == "+") == tensor_index_positions(x)[ind$i])
  )

  tensor_asym(x, ind) |>
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

      # we still need to check if the request makes sense
      # check that "from" indices actually exist in `x`
      # indices must exist
      stopifnot(l$ind_from$i %in% tensor_index_names(tens))
      # index positions must match
      stopifnot(
        all((l$ind_from$p == "+") == tensor_index_positions(tens)[l$ind_from$i])
      )

      tensor_kron(tens, l$ind_from, l$ind_to) |>
        tensor_reduce()
    },
    exprs,
    init = x
  )
}

tensor_subst <- function(x, ind_from, ind_to) {
  names <- tensor_index_names(x)
  names[match(ind_from$i, names)] <- ind_to$i

  new_tensor(
    x,
    names,
    tensor_index_positions(x)
  )
}

tensor_raise <- function(x, ind_from, g) {
  if (is.null(g)) {
    n <- tensor_dim(x, ind_from$i)
    g <- diag(1, n, n)
  }

  if (is.function(g)) {
    g <- g()
  }

  tensor_subst(
    x * new_tensor(solve(g), c(ind_from$i, "?"), c(TRUE, TRUE)),
    list(i = "?", p = "+"), ind_from
  )
}

tensor_lower <- function(x, ind_from, g) {
  if (is.null(g)) {
    n <- tensor_dim(x, ind_from$i)
    g <- diag(1, n, n)
  }

  if (is.function(g)) {
    g <- g()
  }

  tensor_subst(
    x * new_tensor(g, c(ind_from$i, "?"), c(FALSE, FALSE)),
    list(i = "?", p = "-"), ind_from
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

tensor_asym <- function(x, ind) {
  # indices must exist
  stopifnot(ind$i %in% tensor_index_names(x))
  # index positions must match
  stopifnot(all((ind$p == "+") == tensor_index_positions(x)[ind$i]))

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
    list(
      i = paste0("?", ind$i),
      p = ind$p
    ),
    ind
  )
}

tensor_sym <- function(x, ind) {
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
  }
  else {
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

ast_kr <- function(x) {
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
  } else {
    stop("todo")
  }
}

ast_subst <- function(x) {
  if (rlang::is_call(x, "<-")) {
    ind_from <- .(!!x[[3]])
    ind_to <- .(!!x[[2]])

    list(
      name = "subst",
      ind_from = ind_from,
      ind_to = ind_to
    )
  } else {
    stop("todo")
  }
}
