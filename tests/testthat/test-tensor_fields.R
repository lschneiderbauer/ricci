test_that("creating g_mink works", {
  expect_tensor_equal(
    g_mink_cart(2) %_% .(i, j),
    diag(c(-1, 1)) %_% .(i, j)
  )

  expect_tensor_equal(
    g_mink_sph(2) %_% .(i, j),
    diag(c(-1, 1)) %_% .(i, j)
  )

  expect_tensor_equal(
    g_mink_sph(1) %_% .(i, j),
    diag(c(-1), 1, 1) %_% .(i, j)
  )

  expect_snapshot(
    g_mink_cart(2) %_% .(i, +j),
    error = TRUE
  )
})

test_that("creating g_eucl works", {
  expect_tensor_equal(
    g_eucl_cart(2) %_% .(i, j),
    diag(c(1, 1)) %_% .(i, j)
  )

  expect_snapshot(
    g_eucl_cart(2) %_% .(i, +j),
    error = TRUE
  )
})

test_that("creating g_ss works", {
  expect_snapshot(
    g_ss(3)
  )

  expect_error(
    g_ss(2)
  )
})

test_that("christoffel works for metric being 1x1 matrix", {
  expect_equal(
    as.numeric(christoffel(g_eucl_cart(1))),
    0
  )
})

test_that("ricci scalar works", {
  skip_if_not_installed("Ryacas")

  expect_equal(
    as.numeric(simplify(ricci_sc(g_eucl_cart(3)))),
    0
  )

  expect_equal(
    as.numeric(simplify(ricci_sc(g_eucl_sph(3)))),
    0
  )

  expect_equal(
    as.numeric(simplify(simplify(ricci_sc(g_sph(2))))),
    2
  )
})

test_that("printing metric fields does not err", {
  expect_output(
    print(g_mink_cart(3))
  )
})
