test_that("XRD sample files are found.", {
  expect_equal(length(xrd.getSampleFiles()), 4)
})

test_that("All XRD sample files can be imported", {
  fileList = xrd.getSampleFiles()
  nLen = 0
  for(f in fileList) {
    nLen = nLen + nrow(xrd.import(f))
  }
  expect_equal(nLen, 13354)
})
