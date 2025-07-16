# "+", "-", "*", "/", "^", "%%", "%/%"
# "&", "|", "!"
# "==", "!=", "<", "<=", ">=", ">"
#' @export
Ops.tensor <- function(e1, e2) {
  call <- function(fun, name) {
    fun(e1, e2,
      arg1 = rlang::caller_arg(e1),
      arg2 = rlang::caller_arg(e2),
      call = rlang::call2(name)
    )
  }

  switch(.Generic,
    "+" = call(tensor_add, "+"),
    "-" = call(tensor_diff, "-"),
    "*" = call(tensor_mul, "*"),
    "/" = call(tensor_div, "/"),
    "==" = call(tensor_eq, "=="),
    NextMethod()
  )
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
    calculus::`%div%`(as.array(x), as.array(y)),
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
      as.vector(x) * as.array(y),
      index_names = tensor_index_names(y),
      index_positions = tensor_index_positions(y),
      reduced = tensor_is_reduced(y)
    )
  } else if (is_scalar(y)) {
    new_tensor(
      as.vector(y) * as.array(x),
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

        # change other indix names to not trigger calculation
        x_ind_einst[setdiff(names(x_ind), common_indices[einsteinable])] <-
          paste0(x_ind_einst[setdiff(names(x_ind), common_indices[einsteinable])], "1")
        y_ind_einst[setdiff(names(y_ind), common_indices[einsteinable])] <-
          paste0(y_ind_einst[setdiff(names(y_ind), common_indices[einsteinable])], "2")

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
