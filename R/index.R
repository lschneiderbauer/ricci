#' @export
`%_%` <- function(x, i) {
  stopifnot(inherits(i, "tensr_indices"))
  tensr(x, i$i, i$p)
}

. <- function(...) {
  exprs <- rlang::enexprs(...)

  parsed <- unlist(lapply(exprs, ast_extr_ind))

  indices <- unname(parsed)
  positions <- names(parsed)

  positions[positions == ""] <- "-"

  structure(
    list(
      i = indices,
      p = positions
    ),
    class = "tensr_indices"
  )
}

ast_extr_ind <- function(x) {
  switch_expr(x,
    # Base cases
    symbol = as.character(x),
    constant = stop_invalid_expr(),

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
