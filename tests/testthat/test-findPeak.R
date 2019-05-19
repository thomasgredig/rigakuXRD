test_that("Test Peak Finder functionality", {
  fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')
  d <- xrd.read.ASC(fn)
  peak.pos = xrd.find.Peak(d$theta, d$I, 38)
  expect_equivalent(peak.pos, 38.21735, tolerance=1e-4)
})


test_that("Test Peak out of range", {
  fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')
  d <- xrd.read.ASC(fn)
  peak.pos = xrd.find.Peak(d$theta, d$I, 500)
  expect_equivalent(peak.pos, NA)
})
