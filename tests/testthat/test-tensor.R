test_that("creating scalar works", {
  expect_true(
    is_scalar(tensor(3))
  )
})

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

test_that("tensor diaginalization fails if dimensions do not agree", {
  arr <- array(1:(2 * 3), c(2, 3))

  expect_snapshot(
    arr %_% .(i, i),
    error = TRUE
  )
})

test_that("convert tensor to array works", {
  arr <- array(1:(2^2), c(2, 2))

  expect_equal(
    arr %_% .(i, j) |> as_a(j, i),
    t(arr)
  )

  expect_equal(
    arr %_% .(i, j) |> as.array(.(j, i)),
    t(arr)
  )

  expect_equal(
    arr %_% .(i, j) |> as.array(),
    arr
  )
})

test_that("convert tensor to array errs if index does not fit", {
  arr <- array(1:(2^2), c(2, 2))

  expect_snapshot(
    arr %_% .(+i, j) |> as_a(j, i),
    error = TRUE
  )

  expect_snapshot(
    arr %_% .(+i, j) |> as_a(j),
    error = TRUE
  )

  expect_no_error(
    arr %_% .(+i, j) |> as_a(j, +i)
  )
})

test_that("print() does not err", {
  expect_output(
    print(array(1) %_% .(i))
  )
})
