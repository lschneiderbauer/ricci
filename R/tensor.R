new_tensor <- function(x, index_names, index_positions,
                       reduced = FALSE, simplified = FALSE) {
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
    simplified = simplified,
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

# generate a new unique dummy index
new_unique_index <- function(index_names, n = 1) {
  if (length(index_names) == 0) {
    return(paste0("d_", 1:n))
  }
  paste0(
    "d",
    paste0(rep("_", max(nchar(index_names))), collapse = ""),
    1:n
  )
}

tensor_new_dummy_index_name <- function(x, n = 1) {
  new_unique_index(tensor_index_names(x), n)
}


#' @param index_names
#'  A character vector of index names / labels.
#' @param index_positions
#'  A character vector of index positions with two allowed
#'  values "+" and "-", for "upper" and "lower" position respectively.
#'  The length of `index_positions` needs to agree with the length of
#'  `index_names`.
#' @param call For internal use only.
#' @export
#' @rdname create-tensor
#' @concept tensor_create
#' @importFrom cli cli_abort
tensor <- function(a, index_names, index_positions, call = NULL) {
  rlang::check_required(a)
  a <- as.array(a)

  if (is.null(call)) {
    call <- rlang::current_env()
  }

  if (is.character(a)) {
    validate_expressions(a, call = call)
  }

  # check if we have a scalar
  # in this case just return a number
  if (length(dim(a)) == 1 && length(a) == 1 && missing(index_names)) {
    return(new_tensor(a, character(), logical(), reduced = TRUE))
  }

  rlang::check_required(index_names)
  rlang::check_required(index_positions)

  stopifnot(is.character(index_names))
  stopifnot(is.character(index_positions))
  stopifnot(length(index_names) == length(index_positions))

  if (length(index_names) != length(dim(a))) {
    cli_abort(
      c(
        "The number of provided indices do not match the array rank",
        x = "{length(index_names)} indices provided for a
              rank {length(dim(a))} array",
        i = "The number of indices needs to match the array rank of
              argument {.arg a}."
      ),
      call = call
    )
  }

  tensor_reduce(
    new_tensor(a, index_names, index_positions == "+"),
    call = call
  )
}


# reduces a tensor by performing contractions
# and diagonal selections
# after a tensor reduction every index name is unique
#' @importFrom cli cli_abort
tensor_reduce <- function(x, call = rlang::caller_env()) {
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

    if (length(unique(dim(x)[w])) > 1) {
      cli_abort(
        c(
          "Attempted diagonal subsetting on slots with different dimensions.",
          x = "Index {.code {i_dup}} is associated to dimension
              {.val {dim(x)[w]}}.",
          i = "Only assign identical index labels to slots with the equal
                dimension."
        ),
        call = call
      )
    }

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

is_scalar <- function(x) {
  if (inherits(x, "tensor")) {
    return(length(attr(x, "index_names")) == 0)
  } else {
    length(x) == 1
  }
}

# outputs a vector of the same length as index_name
tensor_dim <- function(x, index_name) {
  ndim <- setNames(dim(x), nm = tensor_index_names(x))
  unname(ndim[index_name])
}

tensor_rank <- function(x) {
  length(dim(x))
}

tensor_is_simplified <- function(x) {
  attr(x, "simplified")
}

tensor_simplify <- function(x, force = FALSE) {
  if (!tensor_is_simplified(x) || force) {
    a <- asimplify(x, force)

    new_tensor(
      a,
      tensor_index_names(x),
      tensor_index_positions(x),
      tensor_is_reduced(x),
      simplified = attr(a, "simplified")
    )
  } else {
    x
  }
}

#' Simplify symbolic expressions
#'
#' Attempts to simplify expressions in an array or tensor.
#' Non-array objects are coerced to arrays with [as.array()].
#'
#' @details
#' Instead of using an explicit call to `simplify()` you also have the option
#' to enable automatic simplification via `option(ricci.auto_simplify = TRUE)`.
#' Note however that this comes at a significant performance cost.
#'
#' This operation requires the [Ryacas](https://r-cas.github.io/ryacas/)
#' package.
#'
#' @param x
#'  A character [array()] or [tensor()] consisting of mathematical expressions.
#' @return A character [array()] or [tensor()] of the same form, potentially
#'  with simplified expressions.
#'
#' @examples
#' simplify("x + y - x")
#' @export
#' @concept eval
simplify <- function(x) {
  # yacas has the strange behavior that "simplify" does
  # not seem to be idempotent, i.e. another application
  # of "simplify" simplifies even further ...
  # an expression that reproduces that:
  # g <- g_eucl_sph(3)
  # a <- c(0, 1, 0)
  #
  # (a %_% .(+k) |> covd(.(+j), g = g) *
  #     e(i,j,k) |> r(i, g = g))

  # let's try to apply as many times as we reach a
  # stable point
  # .. really annoying performance wise,
  # but since this is an explicit call, we try to live with it

  fixpoint <- FALSE
  res <- simplify1(x)

  while (!fixpoint) {
    res2 <- simplify1(res)

    if (all(res == res2)) {
      fixpoint <- TRUE
    } else {
      res <- res2
    }
  }

  res
}

simplify1 <- function(x) {
  if (inherits(x, "tensor")) {
    tensor_simplify(x, force = TRUE)
  } else {
    asimplify(as.array(x), force = TRUE)
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

asimplify <- function(a, force = FALSE) {
  if (force && !rlang::is_installed("Ryacas")) {
    rlang::check_installed("Ryacas")
  }

  if (isTRUE(getOption("ricci.auto_simplify")) && !rlang::is_installed("Ryacas")) {
    cli::cli_warn(
      c(
        "Option \"ricci.auto_simplify\" is set to `TRUE`, but the required
        package Ryacas is not installed.",
        i = "Install {{Ryacas}} to get simplification support, e.g. via
            {.run install.packages(\"Ryacas\")}."
      ),
      .frequency = "regularly",
      .frequency_id = "ryacas"
    )
  }

  if (is.character(a) && rlang::is_installed("Ryacas") &&
    (force || isTRUE(getOption("ricci.auto_simplify")))) {
    structure(
      array(
        Ryacas::as_y(a) |>
          Ryacas::y_fn("Simplify") |>
          Ryacas::yac_str() |>
          Ryacas::as_r(),
        dim(a)
      ),
      simplified = TRUE
    )
  } else {
    structure(
      a,
      simplified = FALSE
    )
  }
}


#' @export
format.tensor <- function(x, ...) {
  header <-
    if (!is_scalar(x)) {
      paste0(
        "<Labeled Array> ",
        "[", paste0(dim(x), collapse = "x"),
        "] .(",
        paste0(
          ifelse(tensor_index_positions(x), "+", "-"),
          tensor_index_names(x),
          collapse = ", "
        ),
        ")"
      )
    } else {
      "<Scalar>"
    }

  header
}

#' @export
print.tensor <- function(x, ...) {
  cat(format(x))
  cat("\n")

  if (length(dim(x)) <= 2) {
    print(as.array(x))
  }

  if (!tensor_is_reduced(x)) {
    print("(not reduced)")
  }
}

#' Strip array labels
#'
#' Converts a [tensor()] to an [array()] by stripping the index labels.
#' An index label order
#' needs to be provided so that the array's [dim()] order is well defined.
#'
#' @inheritParams as_a
#' @param index_order
#'  An index specification created with [.()].
#'  The specification needs to match all the labels occurring in `x`.
#'  The label order determines the dimension
#'  order of the resulting array.
#' @param ... Not used.
#' @param arg,call Used for error handling. Can be ignored by the user.
#' @examples
#' array(1:8, dim = c(2, 2, 2)) %_% .(i, +i, k) |> as.array(.(k))
#' @export
#' @concept tensor_dest
as.array.tensor <- function(x, index_order = NULL, ...,
                            arg = "index_order",
                            call = rlang::caller_env()) {
  if (!is.null(index_order)) {
    tensor_validate_index_matching(
      x, index_order,
      match_all = TRUE,
      arg = arg, call = call
    )

    x <- tensor_reorder(x, index_order$i)
  }

  x <- unclass(x)

  attr(x, "index_names") <- NULL
  attr(x, "index_positions") <- NULL
  attr(x, "reduced") <- NULL
  attr(x, "simplified") <- NULL

  x
}

#' Strip array labels
#'
#' Converts a [tensor()] to an [array()] by stripping the index labels.
#' An index label order
#' needs to be provided so that the array's [dim()] order is well defined.
#'
#' @param x  A labeled array ("tensor" object) created by [`%_%`] or [tensor()].
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
#' @examples
#' array(1:8, dim = c(2, 2, 2)) %_% .(i, +i, k) |> as_a(k)
#' @export
#' @seealso The same functionality is implemented in [as.array.tensor()]
#'  but with standard evaluation.
#' @concept tensor_dest
as_a <- function(x, ...) {
  as.array(x, .(...), arg = "...")
}


tensor_alignable <- function(x, y) {
  setequal(tensor_index_names(x), tensor_index_names(y)) &&
    all(tensor_index_positions(x)[tensor_index_names(y)] == tensor_index_positions(y)) &&
    all(tensor_dim(x, tensor_index_names(x)) == tensor_dim(y, tensor_index_names(x)))
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
