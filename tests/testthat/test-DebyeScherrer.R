test_that("get Debye-Scherrer width", {
  fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')
  d <- xrd.read.ASC(fn)
  q = xrd.get.PeakStats(d$theta, d$I, 38.2)

  # expect Au layer to be 14.7nm
  expect_equivalent(xrd.get.DebyeScherrer(q), 147.5, tolerance=1e-4)

})
