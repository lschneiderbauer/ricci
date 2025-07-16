test_that(".() errs correctly", {
  expect_snapshot(
    array(1) %_% "test",
    error = TRUE
  )

  expect_snapshot(
    .(f(y)),
    error = TRUE
  )
})
