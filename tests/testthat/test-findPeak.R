test_that("Test Peak Finder functionality", {
  fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')
  d <- xrd.read.ASC(fn)
  peak.pos = xrd.find.Peak(d$theta, d$I, 38)
  expect_equivalent(peak.pos, 38.24, tolerance=1e-3)
})


test_that("Test Peak out of range", {
  fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')
  d <- xrd.read.ASC(fn)
  peak.pos = xrd.find.Peak(d$theta, d$I, 500)
  expect_equivalent(peak.pos, NA)
})


test_that("Test Peak Width Functionality", {
  fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')
  d <- xrd.read.ASC(fn)
  q = xrd.get.PeakStats(d$theta, d$I, 38.2)
  expect_equivalent(q[4], 0.2346, tolerance=1e-3)
})

test_that("Test Peak Statistics", {
  filename = xrd.getSampleFiles()[1]
  d = xrd.read.ASC(filename)
  q = xrd.get.PeakStats(d$theta, d$I, 38.2)
  expect_equivalent(q[3], 38.20403) # peak pos
  expect_equivalent(q[2], 440.9358) # peak amplitude
  expect_equivalent(q[1], 873.5825) # peak background
})
