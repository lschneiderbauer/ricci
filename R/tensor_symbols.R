#' Kronecker delta
#'
#' Provides a labeled generalized Kronecker delta. In the special
#' case of two labels this represents simply the identity matrix.
#' The Kronecker delta always has an even number of indices.
#' Note that the first half of the tensor labels need to be lowered,
#' while the second half needs upper indices. Otherwise an error is thrown.
#'
#' @param n The dimension.
#' @return
#'  A function that expects index labels (see [.()]) and returns a
#'  labeled tensor. The underlying data will differs depending on
#'  the number of labels provided.
#'
#' @seealso Underlying implementation: [calculus::delta()]
#' @seealso Wikipedia: [Generalized Kronecker delta](https://en.wikipedia.org/wiki/Kronecker_delta#generalized_Kronecker_delta)
#' @examples
#' d(3)(i, +j)
#'
#' d(3)(i, j, +k, +l)
#' @export
#' @concept tensor_symbols
#' @family tensor symbols
d <- function(n) {
  call <- rlang::current_call()
  function(...) {
    i <- .(...)
    order <- length(i$i)

    if (order %% 2 != 0) {
      cli_abort(
        c(
          "Wrong number of indices.",
          x = "Number of indices: {length(i$i)}.",
          i = "The Kronecker delta only accepts an even number of indices."
        )
      )
    }
    p <- order %/% 2

    # we require the first half of the indices to be lower,
    # the other half to be upper
    validate_index_position(
      i, c(rep("-", p), rep("+", p)),
      arg = "...",
      info = "The generalized Kronecker delta requires the first half of
            indices to be lowered, and the second half to be raised.
            If other index structures are required, lower/raise them explicitely
            using a metric tensor, e.g. via {.fun ricci::r}, or {.fun ricci::l}.",
      call = call
    )

    tensor(
      calculus::delta(n = n, p = order %/% 2),
      i$i,
      i$p
    )
  }
}

#' Levi-Civita epsilon
#'
#' Provides a labeled Levi-Civita epsilon (pseudo) tensor.
#' The indices are required to be lowered. Otherwise an error is thrown.
#'
#' @inheritParams .
#' @return
#'  A labeled tensor object.
#'  The underlying data will differs depending on
#'  the number of labels provided.
#'
#' @seealso Underlying implementation: [calculus::epsilon()]
#' @examples
#' e(i, j)
#'
#' e(i, j, k)
#' @seealso Wikipedia: [Levi-Civita symbol](https://en.wikipedia.org/wiki/Levi-Civita_symbol)
#' @export
#' @concept tensor_symbols
#' @family tensor symbols
e <- function(...) {
  i <- .(...)

  validate_index_position(
    i, "-",
    arg = "...",
    info = "The Levi-Civita epsilon can only have lower indices.
            If raised indices are required, raise them explicitely
            using a metric tensor, e.g. via {.fun ricci::r}."
  )

  tensor(
    calculus::epsilon(length(i$i)),
    i$i,
    i$p
  )
}
