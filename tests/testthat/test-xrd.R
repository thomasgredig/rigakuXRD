test_that("XRD reader", {
  filename = system.file("extdata", "MnPcTheta.txt", package='rigakuXRD')
  d = xrd.read.TXT(filename)
  d = d[[1]]

  expect_equal(nrow(d), 2201)
})
