test_that("Estimate Peak Positions", {
  filename  <-  xrd.getSampleFiles(fileExt = 'asc')
  d <- xrd.import(filename, xrd = TRUE)

  peakPos <- c()
  for(j in 1:7) {
    d1 <- xrd_filter(d, (j-1)*10+2, j*10+2)
    p <- xrd.peakEstimate(d1)
    peakPos <- c(peakPos, p$th0)
  }

  # plot(d$theta, d$I, log='y', col='red'); abline(v=peakPos,lwd=2)

  expect_equal(peakPos,
               c(4.00000,15.68421,30.94737,
                 38.31579,44.63158,61.47368,69.36842),
               tolerance = 1e-4)
})
