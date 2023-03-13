test_that("check peak prominence", {
  filename = xrd.getSampleFiles(fileExt='asc')[1]
  d = xrd.read.ASC(filename)
  q = xrd.get.PeakStats(d$theta, d$I, 38.2,
                        Try.Sigma = 0.3, verbose = FALSE)
  p = xrd.get.PeakProminence(q)
  expect_equal(p, 50, tolerance = 0.01)
})

#
# test_that("get all peaks", {
#   filename = xrd.getSampleFiles()[1]
#   d = xrd.read.ASC(filename)
#   q = xrd.get.AllPeaks(d$theta, d$)
# })

