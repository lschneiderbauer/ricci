test_that("tensor kronecker product works", {
  arr <- array(1:(2^2), c(2, 2))

  expect_tensor_equal(
    arr %_% .(i, j) |> kron(.(i, j) -> k),
    as.array(1:4) %_% .(k)
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
  arr <- as.array(c(1, 2, 3, 4))

  expect_equal(
    arr %_% .(i) |> r(i) |> as_a(+i),
    as.array(arr)
  )

  expect_equal(
    arr %_% .(i) |> r(i, g = g_mink(4)) |> as_a(+i),
    as.array(c(-1, 2, 3, 4))
  )

  expect_equal(
    arr %_% .(i) |>
      r(i, g = g_eucl_sph(4)) |>
      as_a(+i) |>
      at(r = 1, ph1 = pi/2, ph2 = pi/2),
    arr
  )

  # we are not allowed to raise already raised index
  expect_snapshot(
    arr %_% .(+i) |> r(i),
    error = TRUE
  )

  expect_snapshot(
    arr %_% .(i) |> r(i, g = g_mink(2)),
    error = TRUE
  )
})

test_that("lowering tensor works", {
  arr <- as.array(c(1, 2, 3, 4))

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
  arr <- array(1:4, dim = c(2, 2))

  expect_tensor_equal(
    arr %_% .(i, j) |> subst(i -> k),
    arr %_% .(k, j)
  )

  expect_tensor_equal(
    arr %_% .(i, j) |> subst(i -> k, j -> l),
    arr %_% .(k, l)
  )

  expect_snapshot(
    arr %_% .(i, +j) |> subst(j -> k),
    error = TRUE
  )

  expect_snapshot(
    arr %_% .(i, j) |> subst(k -> l),
    error = TRUE
  )
})

test_that("antisymmetrized matrices are antisymmetric", {
  arr <- array(1:25, dim = c(5, 5))

  symmat <- as.array(arr %_% .(i, j) |> asym(i, j), .(i, j))

  expect_equal(
    symmat,
    -t(symmat)
  )
})

test_that("antisymmetrization on rank 3 arrays works", {
  arr <- array(1:3^3, dim = c(3, 3, 3))

  symarr <- arr %_% .(i, j, k) |>
    asym(i, j, k) |>
    as_a(i, j, k)

  expect_tensor_equal(
    symarr %_% .(i, j, k),
    -symarr %_% .(j, i, k)
  )

  expect_tensor_equal(
    symarr %_% .(i, j, k),
    -symarr %_% .(i, k, j)
  )

  expect_tensor_equal(
    symarr %_% .(i, j, k),
    symarr %_% .(k, i, j)
  )

  expect_tensor_equal(
    e(i, j, k) |> asym(i, j, k),
    e(i, j, k)
  )
})

test_that("symmetrized matrices are symmetric", {
  arr <- array(1:25, dim = c(5, 5))
  symmat <- as.array(arr %_% .(i, j) |> sym(i, j), .(i, j))

  expect_equal(
    arr %_% .(i, j) |> sym(i, j),
    (arr %_% .(i, j) + arr %_% .(j, i)) / 2
  )

  expect_equal(
    symmat,
    t(symmat)
  )
})

test_that("symmetrization on rank 3 arrays works", {
  arr <- array(1:3^3, dim = c(3, 3, 3))

  symarr <- arr %_% .(i, j, k) |>
    sym(i, j, k) |>
    as_a(i, j, k)

  expect_tensor_equal(
    symarr %_% .(i, j, k),
    symarr %_% .(j, i, k)
  )

  expect_tensor_equal(
    symarr %_% .(i, j, k),
    symarr %_% .(i, k, j)
  )

  expect_tensor_equal(
    symarr %_% .(i, j, k),
    symarr %_% .(k, i, j)
  )
})

test_that("tensor = sym + antisym (two indices)", {
  arr <- array(as.numeric(1:25), dim = c(5, 5))
  tens <- arr %_% .(i, j)

  expect_tensor_equal(
    (tens |> sym(i, j)) +
      (tens |> asym(i, j)),
    tens
  )
})

