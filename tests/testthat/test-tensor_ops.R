test_that("tensor kronecker product works", {
  arr <- array(1:(2^2), c(2, 2))

  expect_tensor_equal(
    arr %_% .(i, j) |> kron(.(i, j) -> k),
    1:4 %_% .(k)
  )

  arr <- array(1:(2^3), c(2, 2, 2))
  expect_tensor_equal(
    arr %_% .(i, j, k) |> kron(.(i, j) -> l),
    array(1:(2^3), c(4, 2)) %_% .(l, k)
  )
})

test_that("kronecker commutes with contraction", {
  arr <- array(runif(4^4), dim = c(4, 4, 4, 4))

  expect_tensor_equal(
    arr %_% .(i, j, +k, +l) |>
      kron(.(i, j) -> m, .(+k, +l) -> +m),
    arr %_% .(i, j, +i, +j)
  )
})

test_that("raising tensor works", {
  arr <- c(1, 2, 3, 4)

  expect_equal(
    arr %_% .(i) |> r(i) |> as_a(+i),
    as.array(arr)
  )

  expect_equal(
    arr %_% .(i) |> r(i, g = g_mink(4)) |> as_a(+i),
    as.array(c(-1, 2, 3, 4))
  )

  # we are not allowed to raise already raised index
  expect_error(
    arr %_% .(+i) |> r(i)
  )
})

test_that("lowering tensor works", {
  arr <- c(1, 2, 3, 4)

  expect_equal(
    arr %_% .(+i) |> l(+i) |> as_a(i),
    as.array(arr)
  )

  expect_equal(
    arr %_% .(+i) |> l(+i, g = g_mink(4)) |> as_a(i),
    as.array(c(-1, 2, 3, 4))
  )

  # we are not allowed to raise already raised index
  expect_error(
    arr %_% .(i) |> l(i)
  )
})

test_that("substituting labels works", {
  arr <- array(1:4, dim = c(2,2))

  expect_tensor_equal(
    arr %_% .(i, j) |> subst(i -> k),
    arr %_% .(k, j)
  )

  expect_tensor_equal(
    arr %_% .(i, j) |> subst(i -> k, j -> l),
    arr %_% .(k, l)
  )

  expect_error(
    arr %_% .(i, +j) |> subst(j -> k)
  )
})

