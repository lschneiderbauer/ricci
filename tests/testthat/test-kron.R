test_that("tensor kronecker product works", {
  arr <- array(1:(2^2), c(2, 2))

  expect_tensor_equal(
    arr %_% .(i, j) |> kr(.(i, j) -> k),
    1:4 %_% .(k)
  )

  arr <- array(1:(2^3), c(2, 2, 2))
  expect_tensor_equal(
    arr %_% .(i, j, k) |> kr(.(i, j) -> l),
    array(1:(2^3), c(4, 2)) %_% .(l, k)
  )
})

test_that("kronecker commutes with ontraction", {
  arr <- array(runif(4^4), dim = c(4, 4, 4, 4))

  expect_tensor_equal(
    arr %_% .(i, j, +k, +l) |>
      kr(.(i, j) -> m, .(+k, +l) -> +m),
    arr %_% .(i, j, +i, +j)
  )
})
