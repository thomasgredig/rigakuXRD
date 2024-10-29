test_that("check peak prominence", {
  filename <- xrd.getSampleFiles(fileExt='asc')
  d <- xrd.import(filename)
  q <- xrd.get.PeakStats(d$TwoTheta, d$I, 38.2,
                        Try.Sigma = 0.3)
  p <- xrd.get.PeakProminence(q)
  expect_equal(p, 50, tolerance = 0.01)
})

