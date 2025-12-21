test_that("XRD fringe thickness", {
  expect_equal(xrd_fringe_thickness(66.22698, 66.32140), 1116.37, tolerance = 0.0001)
  expect_equal(xrd_fringe_thickness(66.69300, 66.78327), 1170.79, tolerance = 0.0001)
})


test_that("XRD Analysis", {
  filename <- xrd_sample_files('rasx')
  data <- xrd_import(filename)
  analysis <- xrd_fringe_analysis(data)
  expect_equal(nrow(analysis$df.peak), 150)
})

test_that("XRD All Peak Search", {
  filename = xrd_sample_files('asc')
  d = xrd_import(filename)
  peak.list = xrd_peak_all(d, delta_theta = 2, try_sigma = c(0.1),  range = c(42, 46))
  expect_equal(length(peak.list), 1)
})

test_that("xrd object creation", {
  filename <- xrd_sample_files('rasx')
  data <- xrd_import(filename, xrd=FALSE)
  expect_true(inherits(data,"data.frame"))

  dataXRD <- create_xrd(data$TwoTheta, data$I, data$I.meas, data$loop)
  expect_true(inherits(dataXRD,"xrd"))
  df <- as.data.frame(dataXRD)
  expect_equal(nrow(df), nrow(data))
})
