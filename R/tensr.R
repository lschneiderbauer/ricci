new_tensr <- function(x, index_names, index_positions, reduced = FALSE) {
  stopifnot(is.array(x))
  stopifnot(is.character(index_names))
  stopifnot(is.logical(index_positions))
  stopifnot(length(index_names) == length(index_positions))
  stopifnot(length(dim(x)) == length(index_names) ||
    (length(dim(x)) == 1 && dim(x) == 1) && length(index_names) == 0)

  # dim is unnamed
  dim(x) <- unname(dim(x))

  structure(
    x,
    index_names = index_names,
    # TRUE = "up", FALSE = "down
    index_positions = index_positions,
    reduced = reduced,
    class = "tensr"
  )
}

tensr_index_names <- function(x) {
  stopifnot(inherits(x, "tensr"))

  attr(x, "index_names")
}

#' @importFrom stats setNames
tensr_index_positions <- function(x) {
  stopifnot(inherits(x, "tensr"))

  setNames(
    attr(x, "index_positions"),
    nm = attr(x, "index_names")
  )
}

tensr_is_reduced <- function(x) {
  stopifnot(inherits(x, "tensr"))

  attr(x, "reduced")
}

#' @export
tensr <- function(x, i) {
  x <- as.array(x)

  # check if we have a scalar
  # in this case just return a number
  if (length(dim(x)) == 1 && length(x) == 1 && is.null(i)) {
    return(new_tensr(x, character(), character()))
  }

  stopifnot("The number of indices does not match the number of array dimensions." = length(i) == length(dim(x)))

  # extract position
  index_names <- gsub("[\\^_]", "", i)
  index_positions <- grepl("^\\^", i)

  tensr_reduce(
    new_tensr(x, index_names, index_positions)
  )
}

# reduces a tensr by performing contractions
# and diagonal selections
# after a tensr reduction every index name is unique
tensr_reduce <- function(x) {
  if (is_scalar(x) || tensr_is_reduced(x)) {
    return(x)
  }

  i <- tensr_index_names(x)
  p <- tensr_index_positions(x)

  # in case we have several indices with equal names
  # and equal position
  # we want to take the "diagonal"
  dups <- duplicated(paste(i, p))

  i_dups <- unique(i[dups])
  for (i_dup in i_dups) {
    w <- which(i == i_dup)
    x <- adiag(x, w)
    # remove this duplicate in i
    ind_dup <- setdiff(w, which.max(i == i_dup))
    i <- i[-ind_dup]
    p <- p[-ind_dup]
  }

  # in case we have the same indices with opposite positions
  # we want to contract it
  # since we got rid of all the same-position-indices in the previous
  # step, we can only have equal named indices with opposite position
  # now, and we do not explicitly have to check for it again
  calculus::index(x) <- i
  x <- as.array(calculus::contraction(x))

  new_tensr(
    x,
    calculus::index(x) %||% character(),
    p[i == calculus::index(x)],
    reduced = TRUE
  )
}

#' @export
`%_%` <- function(x, i) {
  tensr(x, i)
}

is_tensr <- function(x) {
  inherits(x, "tensr")
}

is_scalar <- function(x) {
  if (inherits(x, "tensr")) {
    return(length(attr(x, "index_names")) == 0)
  } else {
    length(x) == 1
  }
}

# picks out "diagonal" indices of an array
# and creates a new array, whose elements
# are a subset of the original array
adiag <- function(x, dims_diag) {
  if (length(dims_diag) == 0) {
    return(x)
  }

  stopifnot(length(unique(dim(x)[dims_diag])) == 1)

  # the following construction only works if we have the "diagonal"
  # components in the first dimensions.
  # we need to reorder if that is not the case
  new_dim_order <- c(dims_diag, setdiff(1:length(dim(x)), dims_diag))
  x_reord <- aperm(x, new_dim_order)

  # we collect the vector indices (i.e. viewing the array as
  # one-dimensional vector), that have equal array indices
  # in the dimensions where the "diagonal" part should be collected

  # this does not make any assumptions on how arrays are indexed
  # vec_ind_keep <-
  #   apply(
  #     arrayInd(seq_along(x_reord), .dim = dim(x_reord))[, seq_along(dims_diag), drop = FALSE],
  #     MARGIN = 1, # over rows,
  #     function(row) {
  #       all(row == row[[1]])
  #     }
  #   )

  # this version does make the assumption of col-major indexing
  # vec_addr = i + (j - 1) * d1 + (k - 1) * d2 * d1.
  # vec_addr = i + (i - 1) * d + (i - 1) * d^2 + ...
  #          = 1 + (i-1) (d^0 + d^1 + d^2) + (l-1) * d^2 * d4 + ...
  d <- dim(x_reord)[[1]]
  i <- 1:d
  vec_ind_keep <- rep(FALSE, d^length(dims_diag))
  vec_ind_keep[1 + (i-1) * sum(d^(seq_along(dims_diag) - 1))] <- TRUE
  vec_ind_keep <-
    rep(
      vec_ind_keep,
      prod(dim(x_reord)[-seq_along(dims_diag)])
    )

  array(
    x_reord[vec_ind_keep],
    dim = dim(x_reord)[-seq_along(dims_diag)[-1]]
  ) |>
    aperm(
      # reestablish original order
      order(rank(new_dim_order[-seq_along(dims_diag)[-1]]))
    )
}


# "+", "-", "*", "/", "^", "%%", "%/%"
# "&", "|", "!"
# "==", "!=", "<", "<=", ">=", ">"
#' @exportS3Method base::Ops
Ops.tensr <- function(e1, e2) {
  switch(.Generic,
    "+" = tensr_add(e1, e2),
    "-" = tensr_diff(e1, e2),
    "*" = tensr_mul(e1, e2),
    "==" = tensr_eq(e1, e2),
    NextMethod()
  )
}

#' @exportS3Method base::print
print.tensr <- function(x, ...) {
  if (!is_scalar(x)) {
    cat(paste0(
      "tensr ",
      "(", paste0(dim(x), collapse = ","), ")",
      paste0(
        ifelse(tensr_index_positions(x), "^", "_"),
        tensr_index_names(x),
        collapse = ""
      ),
      " #", length(x), "\n"
    ))
  } else {
    cat("Scalar\n")
  }

  if (length(dim(x)) <= 2) {
    print(as.array(x))
  }

  if (!tensr_is_reduced(x)) {
    print("(not reduced)")
  }
}

#' @export
# dim order: allows to specify a dimension order by index names
as.array.tensr <- function(x, dim_order = NULL, ...) {
  if (!is.null(dim_order)) {
    stopifnot(setequal(dim_order, tensr_index_names(x)))

    x <- tensr_reorder(x, dim_order)
  }

  x <- unclass(x)

  attr(x, "index_names") <- NULL
  attr(x, "index_positions") <- NULL
  attr(x, "reduced") <- NULL

  x
}

tensr_eq <- function(x, y) {
  stopifnot("tensrs not compatible" = tensr_alignable(x, y))

  all(unclass(x) == unclass(tensr_align(y, x)))
}

tensr_add <- function(x, y) {
  stopifnot("tensrs not compatible" = tensr_alignable(x, y))

  new_tensr(
    calculus::`%sum%`(as.array(x), as.array(tensr_align(y, x))),
    index_names = tensr_index_names(x),
    index_positions = tensr_index_positions(y)
  )
}

tensr_diff <- function(x, y) {
  stopifnot("tensrs not compatible" = tensr_alignable(x, y))

  new_tensr(
    calculus::`%diff%`(as.array(x), as.array(tensr_align(y, x))),
    index_names = tensr_index_names(x),
    index_positions = tensr_index_positions(y)
  )
}

#' @importFrom stats setNames
tensr_mul <- function(x, y) {
  # perform Einstein summation if applicable

  if (is_scalar(x)) {
    new_tensr(
      as.vector(x) * as.array(y),
      index_names = tensr_index_names(y),
      index_positions = tensr_index_positions(y),
      reduced = tensr_is_reduced(y)
    )
  } else if (is_scalar(y)) {
    new_tensr(
      as.vector(y) * as.array(x),
      index_names = tensr_index_names(x),
      index_positions = tensr_index_positions(x),
      reduced = tensr_is_reduced(x)
    )
  } else {
    stopifnot(inherits(x, "tensr"))
    stopifnot(inherits(y, "tensr"))

    if (!tensr_is_reduced(x)) {
      x <- tensr_reduce(x)
    }

    if (!tensr_is_reduced(y)) {
      y <- tensr_reduce(y)
    }

    # only sum over lower and upper indices
    common_indices <- intersect(tensr_index_names(x), tensr_index_names(y))

    einsteinable <-
      vapply(
        common_indices,
        function(ind) {
          x_pos <- tensr_index_positions(x)[which(ind == tensr_index_names(x))]
          y_pos <- tensr_index_positions(y)[which(ind == tensr_index_names(y))]
          xor(x_pos, y_pos)
        },
        FUN.VALUE = FALSE
      )

    x_ind <- setNames(nm = tensr_index_names(x))
    y_ind <- setNames(nm = tensr_index_names(y))
    x_pos <- tensr_index_positions(x)
    y_pos <- tensr_index_positions(y)

    res0 <-
      if (length(common_indices[einsteinable]) == 0) {
        calculus::index(x) <- x_ind
        calculus::index(y) <- y_ind

        new_tensr(
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

        new_tensr(
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
    tensr_reduce(res0)
  }
}

# generalized kronecker product: combines two (or more) indices i,j to a
# single index k where dim(k) = dim(i)*dim(j)
tensr_kron <- function(x, ind_comb, ind_new) {
  stopifnot(inherits(x, "tensr"))
  stopifnot(ind_comb %in% tensr_index_names(x))
  # we require the combining indices to have the same position
  ind_new_pos <- unique(tensr_index_positions(x)[ind_comb])
  stopifnot(length(ind_new_pos) == 1)

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

  ind_invariant <- setdiff(tensr_index_names(x), ind_comb)

  # move combining indices to the beginning
  x <- tensr_reorder(x, c(ind_comb, ind_invariant))

  # reduce dimensions
  dim(x) <-
    c(
      prod(dim(x)[seq_along(ind_comb)]),
      dim(x)[-seq_along(ind_comb)]
    )

  new_tensr(
    x,
    index_names = c(ind_new, ind_invariant),
    index_positions = c(ind_new_pos, tensr_index_positions(x)[ind_invariant])
  ) |>
    tensr_reduce()
}

tensr_alignable <- function(x, y) {
  dx <- setNames(dim(x), tensr_index_names(x))
  dy <- setNames(dim(y), tensr_index_names(x))

  setequal(tensr_index_names(x), tensr_index_names(y)) &&
    all(tensr_index_positions(x)[tensr_index_names(y)] == tensr_index_positions(y)) &&
    all(dx[tensr_index_names(y)] == dy)
}

# aligns y dimensions to x
tensr_align <- function(y, x) {
  if (!setequal(tensr_index_names(x), tensr_index_names(y))) {
    stop("non-conformable arrays")
  }

  z <- tensr_reorder(y, tensr_index_names(x))

  if (any(dim(x) != dim(z))) {
    stop("non-conformable arrays")
  }

  z
}

tensr_reorder <- function(x, new_index_names) {
  stopifnot(inherits(x, "tensr"))
  stopifnot(setequal(tensr_index_names(x), new_index_names))

  x_arr <- as.array(x)

  new_tensr(
    aperm(x_arr, perm = match(new_index_names, tensr_index_names(x))),
    index_names = new_index_names,
    index_positions = tensr_index_positions(x)[new_index_names],
    reduced = tensr_is_reduced(x)
  )
}

#' @export
expect_tensr_equal <- function(object, expected) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package testthat required.")
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package rlang required.")
  }
  if (!requireNamespace("waldo", quietly = TRUE)) {
    stop("Package waldo required.")
  }

  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  exp <- testthat::quasi_label(rlang::enquo(expected), arg = "expected")

  # 2. Call expect()
  act$n <- length(act$val)
  testthat::expect(
    object == expected,
    waldo::compare(object, tensr_align(expected, object),
      x_arg = act$lab, y_arg = exp$lab
    )
  )

  # 3. Invisibly return the value
  invisible(act$val)
}
