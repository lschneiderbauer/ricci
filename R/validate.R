#' @importFrom cli cli_abort
tensor_validate_index_matching <-
  function(x, ind, match_all = FALSE,
           arg = rlang::caller_arg(ind),
           call = rlang::caller_env()) {
    stopifnot(inherits(x, "tensor"))

    if (!inherits(ind, "tensor_indices")) {
      cli_abort(
        c(
          "Argument {.arg {arg}} is not specified correctly.",
          i = "Use {.fun ricci::.} to specify an index set."
        ),
        call = call
      )
    }


    if (!all(ind$i %in% tensor_index_names(x))) {
      missing_ind <- setdiff(ind$i, tensor_index_names(x))

      cli_abort(
        c(
          "Argument {.arg {arg}} contains invalid index.",
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
          "Argument {.arg {arg}} contains index with incorrect position.",
          x = "Position of index {.code {pos_issue_ind}} do{?es/} not match index
              position{?s} in {format(x)}.",
          i = "Make sure you explicitely specify the correct index position."
        ),
        call = call
      )
    }

    # we require that all indices are selected
    if (match_all && !setequal(tensor_index_names(x), ind$i)) {
      missing_ind <- setdiff(tensor_index_names(x), ind$i)

      cli_abort(
        c(
          "Argument {.arg {arg}} does not contain all indices.",
          x = "Index {.code {missing_ind}} missing.",
          i = "Operation requires the explicit selection of all indices."
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

    tensor_validate_index_dim(x, y, arg1, arg2, call)

    stop("Unkown error. Please open a bug report with a reproducible example.")
  }
}

tensor_validate_index_dim <- function(x, y,
                                      arg1 = rlang::caller_arg(x),
                                      arg2 = rlang::caller_arg(y),
                                      call = rlang::caller_env()) {
  # if indices have the same name,
  # they should have the same dimension

  shared_ind <- intersect(tensor_index_names(x), tensor_index_names(y))

  dim_equal <-
    tensor_dim(x, shared_ind) == tensor_dim(y, shared_ind)

  if (!all(dim_equal)) {
    ind_faulty <- shared_ind[!dim_equal]

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
}

#' @importFrom cli cli_abort
stop_invalid_expr <- function(expr,
                              info = info,
                              arg = rlang::caller_arg(expr),
                              call = rlang::caller_env()) {
  cli_abort(
    c(
      "Invalid expression in {.arg {arg}}.",
      x = "Expression {.code {format(expr)}} cannot be parsed.",
      i = info
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
      if (length(pos) > 1) {
        affected_pos <- pos[ind$p != pos]
      } else {
        affected_pos <- pos
      }

      incorrect_state <-
        ifelse(affected_pos == "+", "lowered", "raised")
      correct_state <-
        ifelse(affected_pos == "-", "lowered", "raised")

      cli_abort(
        c(
          "Argument {.arg {arg}} constains index with invalid position.",
          x = "Index {.code {affected_ind}} {?is/are} {incorrect_state}
               while {?it/they} should be {correct_state}.",
          i = info
        ),
        call = call
      )
    }
  }

#' @importFrom cli cli_abort
validate_metric_tensor_indices <-
  function(i, call = rlang::caller_env()) {
    if (length(unique(i$p)) > 1) {
      cli_abort(
        c(
          "The metric tensor indices cannot be mixed",
          i = "Indices either must be all lowered or all raised."
        ),
        call = call
      )
    }
  }

#' @importFrom cli cli_abort
validate_expressions <- function(exprs, arg = rlang::caller_arg(exprs),
                                 call = rlang::caller_env()) {
  rlang::try_fetch(
    rlang::parse_exprs(exprs),
    error = function(cnd) {
      cli_abort(
        c(
          "Invalid expression.",
          i = "Argument {.arg {arg}} needs to contain valid R expressions."
        ),
        parent = cnd,
        call = call
      )
    }
  )
}

validate_coord_expressions <- function(exprs, coords,
                                       arg_exprs = rlang::caller_arg(exprs),
                                       arg_coords = rlang::caller_arg(coords),
                                       call = rlang::caller_env()) {
  # check if we can evaluate that
  rlang::try_fetch(
    calculus::evaluate(
      exprs,
      setNames(
        rep.int(0, length(coords)),
        coords
      )
    ),
    error = function(cnd) {
      cli_abort(
        c(
          "Expression in {.arg {arg_exprs}} is not solely a function of coordinates
          sepcified in {.arg {arg_coords}}.",
          i = "The expression in {.arg {arg_exprs}} can only depend on coordinates,
            not on other parameters. When evaluted at a point (specified by
            coorindates) the expression needs to yield a number."
        ),
        call = call,
        parent = cnd
      )
    }
  )
}


#' @importFrom cli cli_abort
validate_metric_field <- function(metric, metric_inv, coords,
                                  arg_metric = rlang::caller_arg(metric),
                                  arg_metric_inv = rlang::caller_arg(metric_inv),
                                  arg_coords = rlang::caller_arg(coords),
                                  call = rlang::caller_env()) {
  if (!is.array(metric) || length(dim(metric)) != 2 ||
    length(unique(dim(metric))) != 1) {
    cli_abort(
      c(
        "Argument {.arg {arg_metric}} is not a valid array.",
        i = "A quadratic (n by n) matrix/array is required."
      ),
      call = call
    )
  }

  if (!is.array(metric_inv) || length(dim(metric_inv)) != 2 ||
    length(unique(dim(metric_inv))) != 1) {
    cli_abort(
      c(
        "Argument {.arg {arg_metric_inv}} is not a valid array.",
        i = "A quadratic (n by n) matrix/array is required."
      ),
      call = call
    )
  }

  if (any(dim(metric) != dim(metric_inv))) {
    cli_abort(
      c(
        "Matrix dimensions of {.arg {arg_metric}} does not match matrix
        dimensions of {.arg {arg_metric_inv}}.",
        i = "{.arg {arg_metric_inv}} needs to be the inverse matrix
            of {.arg {arg_metric}}."
      ),
      call = call
    )
  }

  if (!is.character(coords)) {
    cli_abort(
      "Argument {.arg {arg_coords}} is no character vector.",
      call = call
    )
  }


  if (!any(unique(dim(metric)) == length(coords))) {
    cli_abort(
      c(
        "Dimension and length of {.arg {arg_metric}} and {.arg {arg_coords}}
        are incompatible.",
        i = "The length of {.arg {arg_coords}} needs to match the array dimension
            of {.arg {arg_metric}}."
      ),
      call = call
    )
  }

  if (is.character(metric)) {
    validate_expressions(metric, call = call)
    validate_coord_expressions(metric, coords, call = call)
  }
  if (is.character(metric_inv)) {
    validate_expressions(metric_inv, call = call)
    validate_coord_expressions(metric_inv, coords, call = call)
  }
}
