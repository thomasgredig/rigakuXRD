library(testthat)

test_that("ASC test file exists", {
  fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')
  expect_true(
    file.exists(fn)
  )
})
