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

  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  exp <- testthat::quasi_label(rlang::enquo(expected), arg = "expected")

  stopifnot(inherits(object, "tensor"))

  # 2. Call expect()
  equal <- object == expected

  comp <-
    if (!equal) {
      waldo::compare(
        act$val, tensor_align(exp$val, act$val),
        ...,
        x_arg = "actual", y_arg = "expected"
      )
    } else {
      list(message = "")
    }

  testthat::expect(
    equal,
    sprintf(
      "%s (`actual`) not equal to %s (`expected`).\n\n%s",
      act$lab, exp$lab,
      paste0(comp, collapse = "\n\n" )
    ),
    trace_env = rlang::caller_env()
  )

  # 3. Invisibly return the value
  invisible(act$val)
}
