test_that("XRD reader", {
  filename = system.file("extdata", "MnPcTheta.txt", package='rigakuXRD')
  # filename=file.path(getwd(),'inst/extdata','MnPcTheta.txt')
  d = xrd.read.TXTnoheader(filename)
  d = d[[1]]

  expect_equal(nrow(d), 2201)
})
