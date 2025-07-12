new_tensor <- function(x, index_names, index_positions, reduced = FALSE) {
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
    class = "tensor"
  )
}

tensor_index_names <- function(x) {
  stopifnot(inherits(x, "tensor"))

  attr(x, "index_names")
}

#' @importFrom stats setNames
tensor_index_positions <- function(x) {
  stopifnot(inherits(x, "tensor"))

  setNames(
    attr(x, "index_positions"),
    nm = attr(x, "index_names")
  )
}

tensor_is_reduced <- function(x) {
  stopifnot(inherits(x, "tensor"))

  attr(x, "reduced")
}


#' @param index_names
#'  A character vector of index names / labels.
#' @param index_positions
#'  A character vector of index positions with two allowed
#'  values "+" and "-", for "upper" and "lower" position respectively.
#'  The length of `index_positions` needs to agree with the length of
#'  `index_names`.
#' @export
#' @rdname create-tensor
#' @concept tensor
tensor <- function(a, index_names, index_positions) {
  a <- as.array(a)
  stopifnot(is.character(index_names))
  stopifnot(is.character(index_positions))
  stopifnot(length(index_names) == length(index_positions))

  # check if we have a scalar
  # in this case just return a number
  if (length(dim(a)) == 1 && length(a) == 1 && is.null(index_names)) {
    return(new_tensor(a, character(), character()))
  }

  if (length(index_names) != length(dim(a))) {
    stop(paste0(
      "The number of indices (", length(index_names),
      ") does not match the number of array dimensions (",
      length(dim(a)),
      ")."
    ))
  }

  tensor_reduce(
    new_tensor(a, index_names, index_positions == "+")
  )
}

# reduces a tensor by performing contractions
# and diagonal selections
# after a tensor reduction every index name is unique
tensor_reduce <- function(x) {
  if (is_scalar(x) || tensor_is_reduced(x)) {
    attr(x, "reduced") <- TRUE
    return(x)
  }

  i <- tensor_index_names(x)
  p <- tensor_index_positions(x)

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

  new_tensor(
    x,
    calculus::index(x) %||% character(),
    p[match(calculus::index(x), i)],
    reduced = TRUE
  )
}

is_tensor <- function(x) {
  inherits(x, "tensor")
}

is_scalar <- function(x) {
  if (inherits(x, "tensor")) {
    return(length(attr(x, "index_names")) == 0)
  } else {
    length(x) == 1
  }
}

tensor_dim <- function(x, index_name) {
  dim(x)[tensor_index_names(x) == index_name]
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
  vec_ind_keep[1 + (i - 1) * sum(d^(seq_along(dims_diag) - 1))] <- TRUE
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
#' @export
Ops.tensor <- function(e1, e2) {
  switch(.Generic,
    "+" = tensor_add(e1, e2),
    "-" = tensor_diff(e1, e2),
    "*" = tensor_mul(e1, e2),
    "==" = tensor_eq(e1, e2),
    NextMethod()
  )
}

#' @export
format.tensor <- function(x, ...) {
  header <-
    if (!is_scalar(x)) {
      paste0(
        "<Labeled Tensor> ",
        "[", paste0(dim(x), collapse = "x"),
        "] <-> .(",
        paste0(
          ifelse(tensor_index_positions(x), "+", "-"),
          tensor_index_names(x),
          collapse = ", "
        ),
        ")\n"
      )
    } else {
      "<Scalar>\n"
    }

  header
}

#' @export
print.tensor <- function(x, ...) {
  cat(format(x))


  if (length(dim(x)) <= 2) {
    print(as.array(x))
  }

  if (!tensor_is_reduced(x)) {
    print("(not reduced)")
  }
}

#' Converts tensor to array
#'
#' Converts tensor to array by stripping the index labels.
#' An index label order
#' needs to be provided so that the array's [dim()] order is well defined.
#'
#' @param index_order
#'  An index specification created with [.()].
#'  The specification needs to match all the labels occurring in `x`.
#'  The label order determines the dimension
#'  order of the resulting array.
#' @examples
#' array(1:8, dim = c(2, 2, 2)) %_% .(i, +i, k) |> .a(k)
#' @export
as.array.tensor <- function(x, index_order = NULL, ...) {
  if (!is.null(index_order)) {
    stopifnot(setequal(index_order$i, tensor_index_names(x)))

    # require position to be correct as well
    # (we force to be explicit about index position in this case)
    # TODO: provide a good error message explaining that

    stopifnot(
      all(tensor_index_positions(x)[index_order$i] ==
        (index_order$p == "+"))
    )

    x <- tensor_reorder(x, index_order$i)
  }

  x <- unclass(x)

  attr(x, "index_names") <- NULL
  attr(x, "index_positions") <- NULL
  attr(x, "reduced") <- NULL

  x
}

#' Strip tensor index labels
#'
#' Converts a labeled tensor to a usual [array()] (hence the name).
#' An index label order
#' needs to be provided so that the array's [dim()] order is well defined.
#'
#' @param x  A labeled tensor object, created by [`%_%`] or [tensor()].
#' @param ...
#'  Index labels separated by commas optionally prefixed by "+" and "-"
#'  to indicate the index position (upper and lower respectively).
#'  If no prefix is provided, a lower index ("-") is assumed.
#'  This argument uses non-standard evaluation: any R symbol
#'  that is not a reserved keyword can be used.
#'  The specification needs to match all the labels occurring in `x`.
#'  The label order determines the dimension
#'  ordering of the resulting array.
#'
#' @return
#'  A usual [array()] without attached labels. The dimension order is
#'  determined by `...`.
#'
#' @export
#' @seealso The same functionality is implemented [as.array.tensor()] but with
#'  standard evaluation.
.a <- function(x, ...) {
  as.array(x, .(...))
}


#' @export
all.equal.tensor <- function(target, current, ...) {
  if (!tensor_alignable(target, current)) {
    all.equal(as.array(target), as.array(current), ...)
  } else {
    all.equal(
      as.array(target),
      as.array(tensor_align(current, target)),
      ...
    )
  }
}

tensor_eq <- function(x, y) {
  stopifnot("tensors not compatible" = tensor_alignable(x, y))

  all(as.array(x) == as.array(tensor_align(y, x)))
}

tensor_add <- function(x, y) {
  stopifnot("tensors not compatible" = tensor_alignable(x, y))

  new_tensor(
    calculus::`%sum%`(as.array(x), as.array(tensor_align(y, x))),
    index_names = tensor_index_names(x),
    index_positions = tensor_index_positions(y)
  )
}

tensor_diff <- function(x, y) {
  stopifnot("tensors not compatible" = tensor_alignable(x, y))

  new_tensor(
    calculus::`%diff%`(as.array(x), as.array(tensor_align(y, x))),
    index_names = tensor_index_names(x),
    index_positions = tensor_index_positions(y)
  )
}

#' @importFrom stats setNames
tensor_mul <- function(x, y) {
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


tensor_alignable <- function(x, y) {
  dx <- setNames(dim(x), tensor_index_names(x))
  dy <- setNames(dim(y), tensor_index_names(x))

  setequal(tensor_index_names(x), tensor_index_names(y)) &&
    all(tensor_index_positions(x)[tensor_index_names(y)] == tensor_index_positions(y)) &&
    all(dx[tensor_index_names(y)] == dy)
}

# aligns y dimensions to x
tensor_align <- function(y, x) {
  if (!setequal(tensor_index_names(x), tensor_index_names(y))) {
    stop("non-conformable arrays")
  }

  z <- tensor_reorder(y, tensor_index_names(x))

  if (any(dim(x) != dim(z))) {
    stop("non-conformable arrays")
  }

  z
}

tensor_reorder <- function(x, new_index_names) {
  stopifnot(inherits(x, "tensor"))
  stopifnot(setequal(tensor_index_names(x), new_index_names))

  x_arr <- as.array(x)

  new_tensor(
    aperm(x_arr, perm = match(new_index_names, tensor_index_names(x))),
    index_names = new_index_names,
    index_positions = tensor_index_positions(x)[new_index_names],
    reduced = tensor_is_reduced(x)
  )
}
