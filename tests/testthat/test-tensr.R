test_that("tensr diagonalization works for matrix", {
  # basically matrix diagonal
  expect_tensr_equal(
    matrix(1:4, 2, 2) %_% c("i", "i"),
    array(c(1, 4), 2) %_% "i"
  )
})

test_that("tensr diagonalization works with in-between dimensions", {
  arr <- array(1:(2^2 * 3), c(2, 3, 2))
  arr_diag <- as.array(arr %_% c("i", "j", "i"))

  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(
        arr_diag[!!i, !!j],
        arr[!!i, !!j, !!i]
      )
    }
  }
})

test_that("tensr diagonalization works with last two dimensions", {
  arr <- array(1:(2^2 * 3), c(3, 2, 2))
  arr_diag <- as.array(arr %_% c("j", "i", "i"))

  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(
        arr_diag[!!j, !!i],
        arr[!!j, !!i, !!i]
      )
    }
  }
})

test_that("tensr diagonalization works with rank four", {
  arr <- array(1:(2^2 * 3^2), c(3, 3, 2, 2))
  arr_diag <- as.array(arr %_% c("j", "k", "i", "i"))

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

test_that("tensr diagonalization works with rank four in-between", {
  arr <- array(1:(2^2 * 3^2), c(3, 2, 3, 2))
  arr_diag <- as.array(arr %_% c("j", "i", "k", "i"))

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

test_that("tensr diagonalization works with first two dimensions", {
  arr <- array(1:(2^2 * 3), c(2, 2, 3))
  arr_diag <- as.array(arr %_% c("i", "i", "j"))

  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(
        arr_diag[!!i, !!j],
        arr[!!i, !!i, !!j]
      )
    }
  }
})

test_that("tensr diagonalization works for two diags at once", {
  arr <- array(1:(2^2 * 3^2), c(2, 3, 2, 3))
  arr_diag <- as.array(arr %_% c("i", "j", "i", "j"))

  for (i in 1:2) {
    for (j in 1:3) {
      expect_equal(
        arr_diag[!!i, !!j],
        arr[!!i, !!j, !!i, !!j]
      )
    }
  }
})

test_that("tensr diagonalization works for three identical indices", {
  arr <- array(1:(3^3), c(3, 3, 3))
  arr_diag <- as.array(arr %_% c("i", "i", "i"))

  for (i in 1:3) {
    expect_equal(
      arr_diag[!!i],
      arr[!!i, !!i, !!i]
    )
  }
})

test_that("tensr multiplication that yields a scalar works", {
  arr <- array(1:2, 2)
  arr_mul <- arr %_% c("i") * arr %_% c("^i")

  expect_true(
    is_scalar(arr_mul)
  )
  expect_equal(
    as.numeric(arr_mul), 5
  )
})

test_that("tensr multiplication that yields a diagonal works", {
  arr <- array(1:2, 2)
  arr_mul <- arr %_% c("i") * arr %_% c("i")

  expect_equal(
    dim(arr_mul),
    2
  )

  expect_equal(
    as.numeric(arr_mul), c(1, 4)
  )
})


test_that("tensr multiplication that yields a diagonal works with singletons", {
  arr_mul <- 1 %_% "i" * 1 %_% "i"

  expect_tensr_equal(
    arr_mul,
    1 %_% "i"
  )
})

test_that("tensr multiplication without summation works", {
  arr <- array(1:(2 * 3), c(2, 3))
  arr_mul <- arr %_% c("i", "j") * arr %_% c("^k", "^l")

  expect_equal(
    dim(arr_mul),
    c(2, 3, 2, 3)
  )

  expect_equal(
    tensr_index_names(arr_mul),
    c("i", "j", "k", "l")
  )

  expect_equal(
    tensr_index_positions(arr_mul),
    c(i = FALSE, j = FALSE, k = TRUE, l = TRUE)
  )
})

test_that("tensr multiplication with scalar works", {
  arr <- array(1:(2 * 3), c(2, 3))
  arr_mul <- arr %_% c("i", "j") * 3

  expect_tensr_equal(
    arr_mul,
    (array(1:(2 * 3), c(2, 3)) * 3) %_% c("i", "j")
  )
})

test_that("tensr addition works", {
  arr <- array(1:(2 * 3), c(2, 3))
  arr_add <- arr %_% c("i", "j") + arr %_% c("i", "j")

  expect_tensr_equal(
    arr_add,
    array(1:(2 * 3) * 2, c(2, 3)) %_% c("i", "j")
  )
})

test_that("tensr addition with reordering works", {
  arr <- array(1:(2^2), c(2, 2))
  arr_add <- arr %_% c("i", "j") + arr %_% c("j", "i")

  expect_tensr_equal(
    arr_add,
    array(1:4 + c(1, 3, 2, 4), c(2, 2)) %_% c("i", "j")
  )
})

test_that("tensr kronecker product works", {
  arr <- array(1:(2^2), c(2, 2))

  expect_tensr_equal(
    tensr_kron(arr %_% c("i", "j"), c("i", "j"), "k"),
    1:4 %_% "k"
  )

  arr <- array(1:(2^3), c(2, 2, 2))
  expect_tensr_equal(
    tensr_kron(arr %_% c("i", "j", "k"), c("i", "j"), "l"),
    array(1:(2^3), c(4,2)) %_% c("l", "k")
  )
})
