# coords: character vector
# metric: covariant metric tensor as array
new_metric_field <- function(metric, coords, metric_inv) {
  structure(
    metric,
    metric_inv = metric_inv,
    coords = coords,
    class = c("metric_field", class(metric))
  )
}

metric_field <- function(metric, metric_inv, coords) {
  stopifnot(is.array(metric))
  stopifnot(is.character(coords))
  stopifnot(unique(dim(metric)) == length(coords))

  # check if we can evaluate that
  calculus::evaluate(
    metric,
    setNames(
      rep.int(0, length(coords)),
      coords
    )
  )

  new_metric_field(metric, coords, metric_inv)
}

metric_coords <- function(metric) {
  stopifnot(inherits(metric, "metric_field"))

  attr(metric, "coords")
}

metric_inv <- function(metric) {
  attr(metric, "metric_inv")
}

#' @export
as.array.metric_field <- function(x, ...) {
  x <- unclass(x)

  attr(x, "coords") <- NULL
  attr(x, "metric_inv") <- NULL

  x
}

#' @export
`%_%.metric_field` <- function(a, i) {
  validate_metric_tensor_indices(i)

  if (length(i$p) > 0 && unique(i$p) == "+") {
    a <- metric_inv(a)
  }

  tensor(a, i$i, i$p)
}

#' @export
print.metric_field <- function(x, ...) {
  cat(format(x))
  cat("\n")
  print(as.array(x))
}

#' @export
format.metric_field <- function(x, ...) {
  paste0(
    "<Covariant metric tensor field> (",
    paste0(metric_coords(x),
      collapse = ", "
    ), ")"
  )
}
