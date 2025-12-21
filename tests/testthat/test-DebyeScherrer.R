test_that("get Debye-Scherrer width", {
  fn = xrd_sample_files(fileExt='asc')
  d <- xrd_import(fn)
  q = xrd_peak_stats(d$TwoTheta, d$I, 38.2, try_sigma = 0.3)

  # expect Au layer to be 14.4nm
  width = xrd_DebyeScherrer(q)
  expect_equal(width[['sigma']], 143.9579, tolerance=1e-3)
})
