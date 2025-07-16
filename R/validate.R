#' @importFrom cli cli_abort
tensor_validate_index_matching <-
  function(x, ind,
           arg = rlang::caller_arg(ind),
           call = rlang::caller_env()) {
    stopifnot(inherits(x, "tensor"))
    stopifnot(inherits(ind, "tensor_indices"))

    if (!all(ind$i %in% tensor_index_names(x))) {
      missing_ind <- setdiff(ind$i, tensor_index_names(x))

      cli_abort(
        c(
          "{.arg {arg}} contains invalid index.",
          x = "Index {.code {missing_ind}} not present in {format(x)}.",
          i = "Make sure you only select indices that match the tensor indices."
        ),
        call = call
      )
    }

    check_position <- (ind$p == "+") == tensor_index_positions(x)[ind$i]
    if (!all(check_position)) {
      pos_issue_ind <-
        paste0(
          ind$p[!check_position],
          ind$i[!check_position]
        )

      cli_abort(
        c(
          "{.arg {arg}} contains index with incorrect position.",
          x = "Position of index {.code {pos_issue_ind}} do{?es/} not match index
              position{?s} in {format(x)}.",
          i = "Make sure you explicitely specify the correct index position."
        ),
        call = call
      )
    }
  }


#' @importFrom cli cli_abort
tensor_validate_alignability <- function(x, y,
                                         arg1 = rlang::caller_arg(x),
                                         arg2 = rlang::caller_arg(y),
                                         call = rlang::caller_env()) {
  stopifnot(inherits(x, "tensor"))
  stopifnot(inherits(y, "tensor"))

  if (!tensor_alignable(x, y)) {
    if (tensor_rank(x) != tensor_rank(y)) {
      cli_abort(
        c(
          "Tensor ranks do not agree.",
          "x" = "Tensor rank of {.arg {arg1}} is {tensor_rank(x)}.",
          "x" = "Tensor rank of {.arg {arg2}} is {tensor_rank(y)}.",
          i = "Operation can only be carried out with two tensors of same rank."
        ),
        call = call
      )
    }

    if (!setequal(tensor_index_names(x), tensor_index_names(y))) {
      diff_x <- setdiff(tensor_index_names(x), tensor_index_names(y))
      diff_y <- setdiff(tensor_index_names(y), tensor_index_names(x))

      cli_abort(
        c(
          "Tensor indices do not agree.",
          "x" = "Tensor index {.code {diff_x}} appear{?s/} in {.arg {arg1}} but
                  not in {.arg {arg2}}.",
          "x" = "Tensor index {.code {diff_y}} appear{?s/} in {.arg {arg2}} but
                  not in {.arg {arg1}}.",
          i = "Operation can only be carried out with two tensors having
                  identical indices."
        ),
        call = call
      )
    }

    pos_equal <-
      tensor_index_positions(x)[tensor_index_names(y)] == tensor_index_positions(y)
    if (!all(pos_equal)) {
      ind_faulty <- tensor_index_names(x)[!pos_equal]

      cli_abort(
        c(
          "Tensor index positions do not agree.",
          "x" = "Tensor index {.code {ind_faulty}} appears with different
                  positions in {.arg {arg1}} and {.arg {arg2}}.",
          i = "Operation can only be carried out with two tensors having
                identical index positions."
        ),
        call = call
      )
    }

    dim_equal <-
      tensor_dim(x, tensor_index_names(x)) == tensor_dim(y, tensor_index_names(x))

    if (!all(dim_equal)) {
      ind_faulty <- tensor_index_names(x)[!dim_equal]

      cli_abort(
        c(
          "Tensor index dimensions do not agree.",
          "x" = "Tensor index {.code {ind_faulty}} ha{?s/ve} dimension{?s}
                    {tensor_dim(x, ind_faulty)} in {.arg {arg1}}.",
          "x" = "Tensor index {.code {ind_faulty}} ha{?s/ve} dimension{?s}
                    {tensor_dim(y, ind_faulty)} in {.arg {arg2}}.",
          i = "Operation can only be carried out with two tensors having
                identical index dimensions."
        ),
        call = call
      )
    }

    stop("error, but no error description provided. Please open a bug.")
  }
}

#' @importFrom cli cli_abort
stop_invalid_expr <- function(expr,
                              arg = rlang::caller_arg(expr),
                              call = rlang::caller_env()) {
  cli_abort(
    c(
      "Invalid expression in {.arg {arg}}.",
      x = "Expression {.code {format(expr)}} cannot be parsed.",
      i = "A valid expressions is of the form
            {{[+|-]<label1>, [+|-]<label2>, ...}}"
    ),
    call = call
  )
}

#' @importFrom cli cli_abort
validate_index_position <-
  function(ind, pos, info,
           arg = rlang::caller_arg(ind),
           call = rlang::caller_env()) {
    stopifnot(inherits(ind, "tensor_indices"))
    stopifnot(all(pos %in% c("+", "-")))

    if (!all(ind$p == pos)) {
      affected_ind <- ind$i[ind$p != pos]
      incorrect_state <-
        ifelse(pos == "+", "lowered", "raised")

      cli_abort(
        c(
          "{.arg {arg}} constains index with invalid position.",
          x = "Index {.code {affected_ind}} {?is/are} {incorrect_state}.",
          i = info
        ),
        call = call
      )
    }
  }
