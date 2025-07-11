test_that("creating d works", {
  expect_tensor_equal(
    d(3)(i, j),
    diag(1, 3, 3) %_% .(i, j)
  )
})

test_that("creating e works", {
  expect_tensor_equal(
    e(i, j),
    array(c(0, -1, 1, 0), dim = c(2,2)) %_% .(i, j)
  )
})

test_that("creating g_mink works", {
  expect_tensor_equal(
    g_mink(2)(i, j),
    diag(c(-1, 1)) %_% .(i, j)
  )

  expect_tensor_equal(
    g_mink(2)(i, +j),
    diag(c(1, 1)) %_% .(i, +j)
  )
})

test_that("creating g_eucl works", {
  expect_tensor_equal(
    g_eucl(2)(i, j),
    diag(c(1, 1)) %_% .(i, j)
  )

  expect_tensor_equal(
    g_mink(2)(i, +j),
    diag(c(1, 1)) %_% .(i, +j)
  )
})
