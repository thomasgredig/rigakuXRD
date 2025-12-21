test_that("check peak prominence", {
  filename <- xrd_sample_files(fileExt='asc')
  d <- xrd_import(filename)
  q <- xrd_peak_stats(d$TwoTheta, d$I, 38.2,
                        try_sigma = 0.3)
  p <- xrd_peak_prominence(q)
  expect_equal(unname(p), 50, tolerance = 0.01)
})

