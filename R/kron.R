#' Kronecker product
#'
#' @param x A labeled tensor object.
#' @inheritParams .
#' @return
#'  A labeled tensor object of class `"tensr"` which is an `array`
#'  whose dimensions have attached labels.
#' @export
kr <- function(x, ...) {
  stopifnot(inherits(x, "tensr"))

  exprs <- rlang::exprs(...)

  Reduce(
    function(tens, expr) {
      l <- ast_extr_ind_assignment(expr)
      tensr_kron(tens, l$ind_comb, l$ind_new)
    },
    exprs,
    init = x
  )
}

ast_extr_ind_assignment <- function(x) {
  if (rlang::is_call(x, "<-") && rlang::is_call(x[[3]], ".")) {
    ind_new <- .(!!x[[2]])
    ind_comb <- eval(x[[3]])

    if (length(ind_new$i) > 1) {
      stop("blah: only one index allowed")
    }
  } else {
    stop("blah")
  }

  list(
    ind_new = ind_new,
    ind_comb = ind_comb
  )
}


# generalized kronecker product: combines two (or more) indices i,j to a
# single index k where dim(k) = dim(i)*dim(j)
tensr_kron <- function(x, ind_comb, ind_new) {
  # indices must exist
  stopifnot(ind_comb$i %in% tensr_index_names(x))
  # index positions must match
  stopifnot(all((ind_comb$p == "+") == tensr_index_positions(x)[ind_comb$i]))
  # we require the combining indices to have the same position
  ind_new_pos <- unique(tensr_index_positions(x)[ind_comb$i])
  stopifnot(length(ind_new_pos) == 1)
  stopifnot((ind_new$p == "+") == ind_new_pos)

  if (!tensr_is_reduced(x)) {
    x <- tensr_reduce(x)
  }

  # R has column major ordering
  # for instance tensr or rank 3 with dim = c(d1, d2, d3)
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

  ind_invariant <- setdiff(tensr_index_names(x), ind_comb$i)

  # move combining indices to the beginning
  x <- tensr_reorder(x, c(ind_comb$i, ind_invariant))

  # reduce dimensions
  dim(x) <-
    c(
      prod(dim(x)[seq_along(ind_comb$i)]),
      dim(x)[-seq_along(ind_comb$i)]
    )

  new_tensr(
    x,
    index_names = c(ind_new$i, ind_invariant),
    index_positions = c(ind_new_pos, tensr_index_positions(x)[ind_invariant])
  ) |>
    tensr_reduce()
}
