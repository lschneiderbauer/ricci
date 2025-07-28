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

#' Create a metric tensor field
#'
#' Metric tensors are an essential ingredient of (Pseudo-) Riemannian
#' manifolds and define distance relations between points.
#' They are used to define geometric tensors such as e.g. the Ricci curvature
#' [ricci()], and a metric connection, i.e. a covariant derivative.
#' They are also essential for raising and lowering indices of tensor fields
#' correctly when using non-flat coordinates.
#'
#' @param metric
#'  A `nxn` matrix / array representing the covariant metric tensor components.
#'  The components are usually expressions as character strings formed from
#'  coordinates, since numeric values can only represent constant tensor fields.
#'
#' @param metric_inv
#'  A `nxn` matrix / array representing the contraviant metric tensor components,
#'  i.e. the inverse matrix of the covariant metric tensor component matrix.
#'
#' @param coords
#'  A character vector of `n` coordinate names that are used in the component
#'  expressions. This information is essential
#'  for forming symbolic derivatives.
#'
#' @seealso Wikipedia: [Metric tensor](https://en.wikipedia.org/wiki/Metric_tensor)
#' @concept metric_tensors
#' @family metric tensors
#'
#' @export
metric_field <- function(metric, metric_inv, coords) {
  rlang::check_required(metric)
  rlang::check_required(metric_inv)
  rlang::check_required(coords)

  validate_metric_field(metric, metric_inv, coords)

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
