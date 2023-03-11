test_that("get Debye-Scherrer width", {
  fn = xrd.getSampleFiles(fileExt='asc')
  d <- xrd.read.ASC(fn)
  q = xrd.get.PeakStats(d$theta, d$I, 38.2)

  # expect Au layer to be 14.7nm
  expect_equivalent(xrd.get.DebyeScherrer(q), 152.2, tolerance=1e-3)
})
