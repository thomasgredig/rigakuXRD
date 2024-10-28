test_that("xrd S3 class", {
  f <- xrd.getSampleFiles('asc')
  d <- xrd.import(f, dataXRD = TRUE)
  expect_true(inherits(d,"xrd"))

  # library(dplyr)
  xrd_filter(d, 32,42) -> d1
  p = xrd.peakEstimate(d1)

  # is there a Au peak ?
  Au_peak <- xrd.find.Peak(d1, peakPos=p$th0)
  expect_equal(Au_peak, 38.21982, tolerance = 1e-5)

  # is there a Si substrate peak ?
  Si_peak <- xrd.find.Peak(d, peakPos = 44, Try.Sigma = c(0.05,0.1) )
  expect_equal(Si_peak, 44.41229, tolerance = 1e-5)
})
