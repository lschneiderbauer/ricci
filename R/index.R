#' Create a labeled tensor
#'
#' Creates a labeled tensor from an array. `%_%` and `tensor()` serve
#' the same purpose, but typically usage of `%_%` is preferred due to
#' brevity.
#' `tensor()` is exported to provide a standard-evaluation interface
#' as well which might be useful under some circumstances.
#'
#' @param a
#'  An array or any object that can be coerced to an array via
#'  [as.array()].
#'
#' @param i
#'  An index slot label specification created with [.()].
#'
#' @return
#'  A labeled tensor object of class `"tensor"`, an [array()]
#'  with attached dimension labels. Note that the index structure
#'  of the resulting tensor does not necessarily have to match `i`. In case
#'  implicit calculations are already triggered (e.g. contractions)
#'  the index structure reflects the resulting tensor.
#'
#' @examples
#' a <- array(1:4, dim = c(2, 2))
#' a %_% .(i, j)
#' @export
#' @rdname create-tensor
#' @concept tensor
`%_%` <- function(a, i) {
  stopifnot(inherits(i, "tensor_indices"))
  tensor(a, i$i, i$p)
}

#' Index slot label specification
#'
#' This function creates a index slot label specification. Any R symbol
#' can serve as a label. `.()` is typically used in conjunction with
#' [%_%].
#'
#' @param ...
#'  Index labels separated by commas optionally prefixed by "+" and "-"
#'  to indicate the index position (upper and lower respectively).
#'  If no prefix is provided, a lower index ("-") is assumed.
#'  This argument uses non-standard evaluation: any R symbol
#'  that is not a reserved keyword can be used.
#'
#' @return
#'  A named list of two character vectors representing the
#'  index label names and index position.
#'
#' @examples
#' # three lower index slots
#' .(i,j,k)
#'
#' # one lower and upper index
#' .(i, +j)
#' @export
#' @concept tensor
. <- function(...) {
  exprs <- rlang::enexprs(...)

  parsed <- unlist(lapply(exprs, ast_extr_ind))

  indices <- unname(parsed)
  positions <- names(parsed)

  positions[positions == ""] <- "-"

  structure(
    list(
      i = indices %||% character(),
      p = positions %||% character()
    ),
    class = "tensor_indices"
  )
}

ast_extr_ind <- function(x) {
  switch_expr(x,
    # Base cases
    symbol = as.character(x),
    constant = as.character(x),

    # Recursive cases
    call =
      if (rlang::is_call(x, "+")) {
        return(c("+" = ast_extr_ind(x[[2]])))
      } else if (rlang::is_call(x, "-")) {
        return(c("-" = ast_extr_ind(x[[2]])))
      } else {
        stop_invalid_expr()
      },
    pairlist = stop_invalid_expr()
  )
}

stop_invalid_expr <- function() {
  stop("Index expression in `.()` not allowed. Only use symbols and '+'/'-' unary operators.", call. = FALSE)
}

switch_expr <- function(x, ...) {
  switch(expr_type(x),
    ...,
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
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
