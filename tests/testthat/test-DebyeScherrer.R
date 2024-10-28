test_that("get Debye-Scherrer width", {
  fn = xrd.getSampleFiles(fileExt='asc')
  d <- xrd.read.ASC(fn)
  q = xrd.get.PeakStats(d$TwoTheta, d$I, 38.2, Try.Sigma = 0.3)

  # expect Au layer to be 14.4nm
  expect_equal(xrd.get.DebyeScherrer(q), 144, tolerance=1e-3)
})
