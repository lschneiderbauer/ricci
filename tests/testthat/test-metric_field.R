test_that("validation works", {
  expect_no_error(
    metric_field(array("a", c(1, 1)), array("1/a", c(1, 1)), "a")
  )

  expect_snapshot(
    metric_field(array("a", c(1, 1)), array("1/a", c(1, 1)), "b"),
    error = TRUE
  )

  expect_snapshot(
    metric_field(array("a", c(1, 1)), array(c("1/a", "b"), c(1, 2)), "a"),
    error = TRUE
  )

  expect_snapshot(
    metric_field(array("a", c(1, 1)), array(1:4, c(2, 2)), "a"),
    error = TRUE
  )

  expect_snapshot(
    metric_field(array("a(", c(1, 1)), array("a", c(1, 1)), "a"),
    error = TRUE
  )

  expect_snapshot(
    metric_field(array(1:6, c(2, 3)), array(1:6, c(2, 3)), "a"),
    error = TRUE
  )

  expect_snapshot(
    metric_field(array("a(", c(1, 1)), array("a", c(1, 1)), 3),
    error = TRUE
  )

  expect_snapshot(
    metric_field(array("a(", c(1, 1)), array("a", c(1, 1)), c("a", "b")),
    error = TRUE
  )
})
