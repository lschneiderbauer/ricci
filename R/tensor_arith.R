#' Arithmetic tensor operations
#'
#' Once a labeled array (tensor) has been defined, tensor arithmetic operations
#' can be carried out with the usual `+`, `-`, `*`, `/`, and `==` symbols.
#'
#' # Addition and Subtraction
#'
#' Addition and subtraction requires the two tensors to have an equal index
#' structure, i.e. the index names their position and the dimensions
#' associated to the index names have to agree.
#' The index order does not matter, the operation will match dimensions
#' by index name.
#'
#' # Multiplication
#'
#' Tensor multiplication takes into account implicit Ricci calculus rules
#' depending on index placement.
#' * Equal-named and opposite-positioned dimensions are contracted.
#' * Equal-named and equal-positioned dimensions are subsetted.
#' * The result is an outer product for distinct index names.
#'
#' # Division
#'
#' Division performs element-wise division. If the second argument is a
#' scalar, each element is simply divided by the scalar.
#' Similar to addition and subtraction, division requires the two tensors
#' to have an equal index structure, i.e. the index names their position
#' and the dimensions associated to the index names have to agree.
#'
#' # Equality check
#'
#' A tensor \eqn{a_{i_1 i_2 ...}} is equal to a tensor \eqn{b_{j_1 j_2 ...}} if
#' and only if the index structure agrees and all components are equal.
#'
#' @param e1,e2 Labeled arrays created with [%_%].
#' @return A resulting labeled array in case of `+`, `-`, `*`, `/`.
#'  `TRUE` or `FALSE` in case of `==`.
#' @examples
#' a <- array(1:4, c(2, 2))
#' b <- array(3 + 1:4, c(2, 2))
#'
#' # addition
#' a %_% .(i, j) + b %_% .(j, i)
#'
#' # multiplication
#' a %_% .(i, j) * b %_% .(+i, k)
#'
#' # division
#' a %_% .(i, j) / 10
#'
#' # equality check
#' a %_% .(i, j) == a %_% .(i, j)
#' a %_% .(i, j) == a %_% .(j, i)
#' a %_% .(i, j) == b %_% .(i, j)
#'
#' \dontrun{
#' # this will err because index structure does not agree
#' a %_% .(i, j) == a %_% .(k, j)
#' }
#' @export
#' @concept arith
#' @family tensor operations
Ops.tensor <- function(e1, e2) {
  call <- function(fun, name) {
    fun(e1, e2,
      arg1 = rlang::caller_arg(e1),
      arg2 = rlang::caller_arg(e2),
      call = rlang::call2(name)
    )
  }

  # Possible operators
  # "+", "-", "*", "/", "^", "%%", "%/%"
  # "&", "|", "!"
  # "==", "!=", "<", "<=", ">=", ">"


  if (missing(e2)) { # check for unary operators
    switch(.Generic,
      "+" = e1,
      "-" = (-1L) * e1,
      NextMethod()
    )
  } else {
    switch(.Generic,
      "+" = call(tensor_simplify %c% tensor_add, "+"),
      "-" = call(tensor_simplify %c% tensor_diff, "-"),
      "*" = call(tensor_simplify %c% tensor_mul, "*"),
      "/" = call(tensor_simplify %c% tensor_div, "/"),
      "==" = call(tensor_eq, "=="),
      NextMethod()
    )
  }
}

tensor_eq <- function(x, y,
                      arg1 = rlang::caller_arg(x),
                      arg2 = rlang::caller_arg(y),
                      call = rlang::caller_env()) {
  tensor_validate_alignability(x, y, arg1, arg2, call)

  all(as.array(x) == as.array(tensor_align(y, x)))
}

tensor_add <- function(x, y,
                       arg1 = rlang::caller_arg(x),
                       arg2 = rlang::caller_arg(y),
                       call = rlang::caller_env()) {
  # before we can properly add tensors we need to reduce them
  if (!tensor_is_reduced(x)) {
    x <- tensor_reduce(x)
  }
  if (!tensor_is_reduced(y)) {
    y <- tensor_reduce(y)
  }

  tensor_validate_alignability(x, y, arg1, arg2, call)

  new_tensor(
    calculus::`%sum%`(as.array(x), as.array(tensor_align(y, x))),
    index_names = tensor_index_names(x),
    index_positions = tensor_index_positions(x),
    # the result is also reduced by definition
    reduced = TRUE
  )
}

tensor_diff <- function(x, y,
                        arg1 = rlang::caller_arg(x),
                        arg2 = rlang::caller_arg(y),
                        call = rlang::caller_env()) {
  # before we can properly add tensors we need to reduce them
  if (!tensor_is_reduced(x)) {
    x <- tensor_reduce(x)
  }
  if (!tensor_is_reduced(y)) {
    y <- tensor_reduce(y)
  }

  tensor_validate_alignability(x, y, arg1, arg2, call)

  new_tensor(
    calculus::`%diff%`(as.array(x), as.array(tensor_align(y, x))),
    index_names = tensor_index_names(x),
    index_positions = tensor_index_positions(x),
    # the result is also reduced by definition
    reduced = TRUE
  )
}

tensor_div <- function(x, y,
                       arg1 = rlang::caller_arg(x),
                       arg2 = rlang::caller_arg(y),
                       call = rlang::caller_env()) {
  # x must be a tensor, but y doesn't need to be

  # before we can properly add tensors we need to reduce them
  if (!tensor_is_reduced(x)) {
    x <- tensor_reduce(x)
  }
  if (inherits(y, "tensor") && !tensor_is_reduced(y)) {
    y <- tensor_reduce(y)
  }

  if (inherits(x, "tensor") && inherits(y, "tensor")) {
    tensor_validate_alignability(x, y, arg1, arg2, call)
  }

  new_tensor(
    array(calculus::`%div%`(as.array(x), as.array(y)), dim(x)),
    index_names = tensor_index_names(x),
    index_positions = tensor_index_positions(x),
    # the result is also reduced by definition
    reduced = TRUE
  )
}

#' @importFrom stats setNames
tensor_mul <- function(x, y,
                       arg1 = rlang::caller_arg(x),
                       arg2 = rlang::caller_arg(y),
                       call = rlang::caller_env()) {
  # perform Einstein summation if applicable

  if (is_scalar(x)) {
    new_tensor(
      calculus::`%prod%`(as.vector(x), as.array(y)),
      index_names = tensor_index_names(y),
      index_positions = tensor_index_positions(y),
      reduced = tensor_is_reduced(y)
    )
  } else if (is_scalar(y)) {
    new_tensor(
      calculus::`%prod%`(as.vector(y), as.array(x)),
      index_names = tensor_index_names(x),
      index_positions = tensor_index_positions(x),
      reduced = tensor_is_reduced(x)
    )
  } else {
    stopifnot(inherits(x, "tensor"))
    stopifnot(inherits(y, "tensor"))

    if (!tensor_is_reduced(x)) {
      x <- tensor_reduce(x)
    }

    if (!tensor_is_reduced(y)) {
      y <- tensor_reduce(y)
    }

    tensor_validate_index_dim(x, y, arg1, arg2, call)

    # only sum over lower and upper indices
    common_indices <- intersect(tensor_index_names(x), tensor_index_names(y))

    einsteinable <-
      vapply(
        common_indices,
        function(ind) {
          x_pos <- tensor_index_positions(x)[which(ind == tensor_index_names(x))]
          y_pos <- tensor_index_positions(y)[which(ind == tensor_index_names(y))]
          xor(x_pos, y_pos)
        },
        FUN.VALUE = FALSE
      )

    x_ind <- setNames(nm = tensor_index_names(x))
    y_ind <- setNames(nm = tensor_index_names(y))
    x_pos <- tensor_index_positions(x)
    y_pos <- tensor_index_positions(y)

    res0 <-
      if (length(common_indices[einsteinable]) == 0) {
        calculus::index(x) <- x_ind
        calculus::index(y) <- y_ind

        new_tensor(
          calculus::`%outer%`(as.array(x), as.array(y)),
          index_names = c(x_ind, y_ind),
          index_positions = c(x_pos, y_pos)
        )
      } else {
        x_ind_einst <- x_ind
        y_ind_einst <- y_ind

        # change other index names to not trigger calculation
        untouched_indices_x <- setdiff(names(x_ind), common_indices[einsteinable])
        untouched_indices_y <- setdiff(names(y_ind), common_indices[einsteinable])
        dummy_index_names <-
          new_unique_index(c(x_ind, y_ind),
            n = length(untouched_indices_x) + length(untouched_indices_y)
          )
        x_ind_einst[untouched_indices_x] <-
          dummy_index_names[seq_along(untouched_indices_x)]

        y_ind_einst[untouched_indices_y] <-
          dummy_index_names[length(untouched_indices_x) + seq_along(untouched_indices_y)]

        calculus::index(x) <- x_ind_einst
        calculus::index(y) <- y_ind_einst

        new_tensor(
          as.array(calculus::einstein(as.array(x), as.array(y))),
          index_names =
            c(
              x_ind[setdiff(names(x_ind), common_indices[einsteinable])],
              y_ind[setdiff(names(y_ind), common_indices[einsteinable])]
            ),
          index_positions =
            c(
              x_pos[setdiff(names(x_ind), common_indices[einsteinable])],
              y_pos[setdiff(names(y_ind), common_indices[einsteinable])]
            )
        )
      }

    # if we have common indices with same position
    # we need to reduce that
    tensor_reduce(res0)
  }
}
