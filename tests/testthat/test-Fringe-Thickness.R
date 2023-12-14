test_that("XRD fringe thickness", {
  expect_equal(xrd.FringeThickness(66.22698, 66.32140), 1116.37, tolerance = 0.0001)
  expect_equal(xrd.FringeThickness(66.69300, 66.78327), 1170.79, tolerance = 0.0001)
})
