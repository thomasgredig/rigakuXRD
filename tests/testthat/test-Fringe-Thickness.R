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
  d = xrd.import(filename)
  peak.list = xrd.get.AllPeaks(d, deltaTheta = 2, Try.Sigma = c(0.1),  Range = c(42, 46))
  expect_equal(length(peak.list), 1)

  d42 = xrd_filter(d, 42.0, 42.8) # not enough coverage
  expect_warning(xrd.get.AllPeaks(d42),"XRD data has insufficient angular range.")

  expect_warning(xrd_filter(d42, 10,8), "max theta is smaller than min theta.")
})

test_that("xrd object creation", {
  filename <- xrd.getSampleFiles('rasx')
  data <- xrd.import(filename, xrd=FALSE)
  expect_true(inherits(data,"data.frame"))

  dataXRD <- create_xrd(data$TwoTheta, data$I, data$I.meas, data$loop)
  expect_true(inherits(dataXRD,"xrd"))
  df <- as.data.frame(dataXRD)
  expect_equal(nrow(df), nrow(data))
})
