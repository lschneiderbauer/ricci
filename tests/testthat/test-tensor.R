test_that("tensor contraction works for rank four", {
  arr <- array(1:(2^2 * 3^2), c(3, 2, 3, 2))
  arr_contr <- as.array(arr %_% .(j, i, k, +i))

  expect_equal(
    arr_contr,
    arr[, 1, , 1] + arr[, 2, , 2]
  )
})

test_that("tensor diagonalization works for matrix", {
  # basically matrix diagonal
  expect_tensor_equal(
    matrix(1:4, 2, 2) %_% .(i, i),
    array(c(1, 4), 2) %_% .(i)
  )
})

test_that("tensor diagonalization works with in-between dimensions", {
  arr <- array(1:(2^2 * 3), c(2, 3, 2))
  arr_diag <- as.array(arr %_% .(i, j, i))

  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(
        arr_diag[!!i, !!j],
        arr[!!i, !!j, !!i]
      )
    }
  }
})

test_that("tensor diagonalization works with last two dimensions", {
  arr <- array(1:(2^2 * 3), c(3, 2, 2))
  arr_diag <- as.array(arr %_% .(j, i, i))

  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(
        arr_diag[!!j, !!i],
        arr[!!j, !!i, !!i]
      )
    }
  }
})

test_that("tensor diagonalization works with rank four", {
  arr <- array(1:(2^2 * 3^2), c(3, 3, 2, 2))
  arr_diag <- as.array(arr %_% .(j, k, i, i))

  for (i in 1:2) {
    for (j in 1:3) {
      for (k in 1:3) {
        expect_equal(
          arr_diag[!!j, !!k, !!i],
          arr[!!j, !!k, !!i, !!i]
        )
      }
    }
  }
})

test_that("tensor diagonalization works with rank four in-between", {
  arr <- array(1:(2^2 * 3^2), c(3, 2, 3, 2))
  arr_diag <- as.array(arr %_% .(j, i, k, i))

  for (i in 1:2) {
    for (j in 1:3) {
      for (k in 1:3) {
        expect_equal(
          arr_diag[!!j, !!i, !!k],
          arr[!!j, !!i, !!k, !!i]
        )
      }
    }
  }
})

test_that("tensor diagonalization works with first two dimensions", {
  arr <- array(1:(2^2 * 3), c(2, 2, 3))
  arr_diag <- as.array(arr %_% .(i, i, j))

  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(
        arr_diag[!!i, !!j],
        arr[!!i, !!i, !!j]
      )
    }
  }
})

test_that("tensor diagonalization works for two diags at once", {
  arr <- array(1:(2^2 * 3^2), c(2, 3, 2, 3))
  arr_diag <- as.array(arr %_% .(i, j, i, j))

  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(
        arr_diag[!!i, !!j],
        arr[!!i, !!j, !!i, !!j]
      )
    }
  }
})

test_that("tensor diagonalization works for three identical indices", {
  arr <- array(1:(3^3), c(3, 3, 3))
  arr_diag <- as.array(arr %_% .(i, i, i))

  for (i in 1:3) {
    expect_equal(
      arr_diag[!!i],
      arr[!!i, !!i, !!i]
    )
  }
})

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
  arr_mul <- 1 %_% .(i) * 1 %_% .(i)

  expect_tensor_equal(
    arr_mul,
    1 %_% .(i)
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
  arr_mul <- arr %_% .(i, j) * 3
  arr_mul2 <- 3 * arr %_% .(i, j)

  expect_tensor_equal(
    arr_mul,
    (array(1:(2 * 3), c(2, 3)) * 3) %_% .(i, j)
  )

  expect_tensor_equal(
    arr_mul2,
    (array(1:(2 * 3), c(2, 3)) * 3) %_% .(i, j)
  )
})

test_that("tensor addition works", {
  arr <- array(1:(2 * 3), c(2, 3))
  arr_add <- arr %_% .(i, j) + arr %_% .(i, j)

  expect_tensor_equal(
    arr_add,
    array(1:(2 * 3) * 2, c(2, 3)) %_% .(i, j)
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


