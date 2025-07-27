test_that("Laplacian works", {
  skip_if_not_installed("withr")
  skip_if_not_installed("Ryacas")

  withr::local_options(ricci.simplify = TRUE)

  expect_tensor_equal(
    covd("1/r", .(k, +k), g = g_eucl_sph(3)),
    tensor("0")
  )

  # expect_tensor_equal(
  #   covd("1/sqrt(x1^2 + x2^2 + x3^2)", .(k, +k), g = g_eucl_cart(3)),
  #   tensor("0")
  # )
})

test_that("covd: act_on argument is working", {
  skip_if_not_installed("withr")
  skip_if_not_installed("Ryacas")

  withr::local_options(ricci.simplify = TRUE)

  arr <- array(1:6, dim = c(2, 3))

  expect_no_error(
    arr %_% .(i, j) |> covd(.(k), act_on = .(i), g = g_eucl_cart(2))
  )

  expect_error(
    arr %_% .(i, j) |> covd(.(k), act_on = .(i), g = g_eucl_cart(3))
  )

  expect_no_error(
    arr %_% .(i, j) |> covd(.(k), act_on = .(j), g = g_eucl_cart(3))
  )

  expect_error(
    arr %_% .(i, j) |> covd(.(k), act_on = .(j), g = g_eucl_cart(2))
  )
})

test_that("covariant derivative of any metric tensor must vanish", {
  skip_if_not_installed("withr")
  skip_if_not_installed("Ryacas")

  withr::local_options(ricci.simplify = TRUE)

  der <- function(g) {
    g %_% .(i, j) |>
      covd(.(k), g = g) |>
      # simplify() |>
      as.array() |>
      as.numeric() |>
      unique()
  }

  expect_equal(
    der(g_eucl_sph(3)),
    0
  )

  expect_equal(
    der(g_eucl_cart(3)),
    0
  )

  expect_equal(
    der(g_mink_sph(3)),
    0
  )
})
