test_that("XRD sample files are found.", {
  expect_equal(length(xrd_sample_files()), 4)
})

test_that("All XRD sample files can be imported", {
  fileList = xrd_sample_files()
  nLen = 0
  for(f in fileList) {
    nLen = nLen + nrow(xrd_import(f))
  }
  expect_equal(nLen, 13354)
})
