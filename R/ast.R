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

ast_extr_ind <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  switch_expr(x,
    # Base cases
    symbol = as.character(x),
    constant = as.character(x),

    # Recursive cases
    call =
      if (rlang::is_call(x, "+")) {
        return(c("+" = ast_extr_ind(x[[2]], arg, call)))
      } else if (rlang::is_call(x, "-")) {
        return(c("-" = ast_extr_ind(x[[2]], arg, call)))
      } else {
        stop_invalid_expr(x, arg = arg, call = call)
      },
    pairlist = stop_invalid_expr(x, arg = arg, call = call)
  )
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
