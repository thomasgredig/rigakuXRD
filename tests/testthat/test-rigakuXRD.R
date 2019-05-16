library(testthat)

test_that("ASC test file exists", {
  fn = system.file("extdata", "2Theta.csv", package = "rigakuXRD")
  expect_true(
    file.exists(fn)
  )
})
