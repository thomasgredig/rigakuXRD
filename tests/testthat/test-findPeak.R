filename = xrd.getSampleFiles('asc')
d = xrd.import(filename)

test_that("Find largest peak", {
  peak.pos = xrd.find.Peak(d$TwoTheta, d$I, 69)
  expect_equal(peak.pos, 69.14, tolerance = 1e-4)
})

test_that("Test Peak Width Fit", {
  q = xrd.get.PeakStats(d$TwoTheta, d$I, 38.2)
  expect_equal(q[4], 0.2346, tolerance=1e-3)
})

test_that("Test Peak Position Fit", {
  q <- xrd.get.PeakStats(d$TwoTheta, d$I, 38.1 )
  expect_equal(q[3], 38.21774, Try.Sigma = 0.3, tolerance=1e-4)
})

test_that("Test Peak Background and Amplitude Fit", {
  q = xrd.get.PeakStats(d$TwoTheta, d$I, 38.2)
  expect_equal(q[2], 422,Try.Sigma = 0.3, tolerance=1e-3) # peak amplitude
  expect_equal(q[1], 890,Try.Sigma = 0.3, tolerance=1e-3) # peak background
})

test_that("No peak at 42 or 72, but warning", {
  filename <- xrd.getSampleFiles("asc")
  d = xrd.import(filename)

  # there is no peak in this data
  expect_warning(xrd.find.Peak(d$TwoTheta, d$I,42, thetaDelta=5),
                 "No Gaussian width.")

  # peak is partial on the left
  expect_warning(xrd.find.Peak(d$TwoTheta, d$I, 72, thetaDelta=5 ),
                 "Peak leftwards.")
})

