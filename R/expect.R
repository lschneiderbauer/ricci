#' Does code return the expected value?
#'
#' Adds an expectation function that can be used with the
#' [testthat](https://testthat.r-lib.org/) package.
#' Compares two tensors and determines whether they are equal or not.
#'
#' @inheritParams testthat::expect_equal
#' @inheritDotParams waldo::compare tolerance max_diffs ignore_srcref
#'                                  ignore_attr ignore_encoding
#'                                  ignore_function_env
#'                                  ignore_formula_env
#'                                  quote_strings
#' @return The actual value invisibly.
#'
#' @export
expect_tensor_equal <- function(object, expected, ...) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop("Package testthat required.") # nocov
  }
  if (!requireNamespace("waldo", quietly = TRUE)) {
    stop("Package waldo required.") # nocov
  }

  stopifnot(inherits(object, "tensor"))

  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  exp <- testthat::quasi_label(rlang::enquo(expected), arg = "expected")

  # 2. Call expect()
  act$n <- length(act$val)
  testthat::expect(
    object == expected,
    waldo::compare(
      object, tensor_align(expected, object),
      x_arg = act$lab, y_arg = exp$lab,
      ...
    )
  )

  # 3. Invisibly return the value
  invisible(act$val)
}
