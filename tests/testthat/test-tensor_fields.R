
test_that("creating g_mink works", {
  expect_tensor_equal(
    g_mink(2) %_% .(i, j),
    diag(c(-1, 1)) %_% .(i, j)
  )

  expect_snapshot(
    g_mink(2) %_% .(i, +j),
    error = TRUE
  )
})

test_that("creating g_eucl works", {
  expect_tensor_equal(
    g_eucl_cart(2) %_% .(i, j),
    diag(c(1, 1)) %_% .(i, j)
  )

  expect_snapshot(
    g_mink(2) %_% .(i, +j),
    error = TRUE
  )
})
