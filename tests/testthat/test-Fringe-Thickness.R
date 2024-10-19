test_that("XRD fringe thickness", {
  expect_equal(xrd.FringeThickness(66.22698, 66.32140), 1116.37, tolerance = 0.0001)
  expect_equal(xrd.FringeThickness(66.69300, 66.78327), 1170.79, tolerance = 0.0001)
})


test_that("XRD Analysis", {
  filename <- xrd.getSampleFiles('rasx')
  data <- xrd.import(filename)
  analysis <- xrd.FringeAnalysis(data)
  expect_equal(nrow(analysis$df.peak), 150)
})

test_that("XRD All Peak Search", {
  filename = xrd.getSampleFiles('asc')
  d = xrd.read.ASC(filename)
  peak.list = xrd.get.AllPeaks(d, deltaTheta = 2, Try.Sigma = c(0.1),  Range = c(42, 46))
  expect_equal(length(peak.list), 3)
})
