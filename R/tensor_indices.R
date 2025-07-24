#' Create a labeled array (tensor)
#'
#' Creates a labeled array (tensor) from an array. `%_%` and `tensor()` serve
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
  if (!inherits(i, "tensor_indices")) {
    cli_abort(
      c(
        "Second argument of {.fun %_%} is not an index specification.",
        i = "Indices need to be specified with {.fun ricci::.}."
      )
    )
  }

  UseMethod("%_%")
}

#' @export
`%_%.array` <- function(a, i) {
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
#' .(i, j, k)
#'
#' # one lower and upper index
#' .(i, +j)
#' @export
#' @concept tensor
. <- function(...) {
  exprs <- rlang::enexprs(...)

  parsed <- unlist(lapply(
    exprs,
    \(x) ast_extr_ind(
      x,
      arg = "...",
      call = rlang::env_parent()
    )
  ))

  indices <- unname(parsed)
  positions <- names(parsed)

  positions[positions == ""] <- "-"

  new_tensor_indices(indices, positions)
}

new_tensor_indices <- function(i, p) {
  structure(
    list(
      i = unname(i) %||% character(),
      p = unname(p) %||% character()
    ),
    class = "tensor_indices"
  )
}
