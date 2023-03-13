test_that("Estimate Peak Positions", {
  filename = xrd.getSampleFiles(fileExt = 'asc')[1]
  d = xrd.read.ASC(filename)

  peakPos = c()
  for(j in 1:7) {
    d1 = d %>% filter(theta < j*10+2 & theta > (j-1)*10+2)
    p = xrd.peakEstimate(d1$theta, d1$I)
    peakPos = c(peakPos, p$th0)
  }

  # plot(d$theta, d$I, log='y', col='red'); abline(v=peakPos,lwd=2)

  expect_equal(peakPos,
               c(4.00000,15.68684,30.93947,
                 38.31316,44.63632,61.99000,69.36368),
               tolerance = 1e-4)
})
