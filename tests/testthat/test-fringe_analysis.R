test_that("check fringe analysis example", {
  filename <- xrd.getSampleFiles('ras')
  data <- xrd.import(filename, xrd=TRUE)
  data_5 = xrd_filter(data, 5,10)
  analysis <- xrd.FringeAnalysis(data_5)
  expect_equal(analysis$t.nm, c(14.97,9.81,16.06), tolerance = 1e-3)
})
