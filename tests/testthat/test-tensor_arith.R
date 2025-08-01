test_that("tensor multiplication that yields a scalar works", {
  arr <- array(1:2, 2)
  arr_mul <- arr %_% .(i) * arr %_% .(+i)

  expect_true(
    is_scalar(arr_mul)
  )
  expect_equal(
    as.numeric(arr_mul), 5
  )
})

test_that("tensor multiplication that yields a diagonal works", {
  arr <- array(1:2, 2)
  arr_mul <- arr %_% .(i) * arr %_% .(i)

  expect_equal(
    dim(arr_mul),
    2
  )

  expect_equal(
    as.numeric(arr_mul), c(1, 4)
  )
})


test_that("tensor multiplication that yields a diagonal works with singletons", {
  arr_mul <- as.array(1) %_% .(i) * as.array(1) %_% .(i)

  expect_tensor_equal(
    arr_mul,
    as.array(1) %_% .(i)
  )
})

test_that("tensor multiplication without summation works", {
  arr <- array(1:(2 * 3), c(2, 3))
  arr_mul <- arr %_% .(i, j) * arr %_% .(+k, +l)

  expect_equal(
    dim(arr_mul),
    c(2, 3, 2, 3)
  )

  expect_equal(
    tensor_index_names(arr_mul),
    c("i", "j", "k", "l")
  )

  expect_equal(
    tensor_index_positions(arr_mul),
    c(i = FALSE, j = FALSE, k = TRUE, l = TRUE)
  )
})

test_that("tensor multiplication with scalar works", {
  arr <- array(1:(2 * 3), c(2, 3))

  expect_tensor_equal(
    arr %_% .(i, j) * 3,
    (arr * 3) %_% .(i, j)
  )

  expect_tensor_equal(
    3 * arr %_% .(i, j),
    (arr * 3) %_% .(i, j)
  )

  arr_sym <- array(rep("a", 6), c(2, 3))

  expect_tensor_equal(
    at(arr_sym %_% .(i, j) * 3, c(a = 1)),
    at(calculus::`%prod%`(arr_sym, 3) %_% .(i, j), c(a = 1))
  )

  expect_tensor_equal(
    at(3 * arr_sym %_% .(i, j), c(a = 1)),
    at(calculus::`%prod%`(arr_sym, 3) %_% .(i, j), c(a = 1))
  )
})


test_that("tensor division by scalar works", {
  arr <- array(1:(2 * 3), c(2, 3))

  expect_tensor_equal(
    arr %_% .(i, j) / 3,
    (arr / 3) %_% .(i, j)
  )

  arr_sym <- array(rep("a", 6), c(2, 3))

  expect_tensor_equal(
    (arr_sym %_% .(i, j) / 3) |> at(c(a = 1)),
    (calculus::`%div%`(arr_sym, 3) |> at(c(a = 1))) %_% .(i, j)
  )

  arr_sing <- array(1, c(1, 1))
  expect_tensor_equal(
    arr_sing %_% .(i, j) / 2,
    (arr_sing / 2) %_% .(i, j)
  )
})

test_that("tensor division by other tensor works", {
  arr <- array(1:(2 * 3), c(2, 3))

  expect_tensor_equal(
    arr %_% .(i, j) / arr %_% .(i, j),
    array(rep(1, 6), c(2, 3)) %_% .(i, j)
  )
})

test_that("tensor multiplication yields correct errors", {
  a <- array(1:4, 4)
  b <- array(1:3, 3)

  expect_snapshot(
    a %_% .(i) * b %_% .(+i),
    error = TRUE
  )
})

test_that("tensor addition works", {
  arr <- array(1:(2 * 3), c(2, 3))
  arr_add <- arr %_% .(i, j) + arr %_% .(i, j)

  expect_tensor_equal(
    arr_add,
    array(1:(2 * 3) * 2, c(2, 3)) %_% .(i, j)
  )

  expect_snapshot(
    arr %_% .(i, j) + arr %_% .(k, l),
    error = TRUE
  )

  arr2 <- array(1:4, 4)
  expect_snapshot(
    arr %_% .(i, j) + arr2 %_% .(i),
    error = TRUE
  )

  expect_snapshot(
    arr %_% .(i, j) + arr %_% .(+i, j),
    error = TRUE
  )
})

test_that("tensor subtraction works", {
  arr <- array(1:(2 * 3), c(2, 3))
  arr_add <- arr %_% .(i, j) - arr %_% .(i, j)

  expect_tensor_equal(
    arr_add,
    array(0, c(2, 3)) %_% .(i, j)
  )
})


test_that("tensor addition with reordering works", {
  arr <- array(1:(2^2), c(2, 2))
  arr_add <- arr %_% .(i, j) + arr %_% .(j, i)

  expect_tensor_equal(
    arr_add,
    array(1:4 + c(1L, 3L, 2L, 4L), c(2, 2)) %_% .(i, j)
  )
})

test_that("tensor with unary operator works", {
  arr <- array(1:(2^2), c(2, 2))

  expect_tensor_equal(
    -(arr %_% .(i, j)),
    (-arr) %_% .(i, j)
  )

  expect_tensor_equal(
    +(arr %_% .(i, j)),
    (+arr) %_% .(i, j)
  )
})
