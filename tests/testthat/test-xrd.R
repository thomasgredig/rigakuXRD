test_that("XRD sample files", {
  file.list = xrd.getSampleFiles()
  expect_equal(length(file.list),3)
})

test_that("Load XRD sample file data", {
  sel.Files=c(1,3)
  file.list = xrd.getSampleFiles()[sel.Files]
  nLen=c()
  for(f in file.list) {
    d = xrd.import(f)
    nLen=c(nLen,nrow(d))
  }
  expect_equal(nLen,c(7601,1401,2201)[sel.Files])
})



test_that("Load XRD sample file data", {
  fname = xrd.getSampleFiles()[1]
  expect_true(grepl('asc$',fname))

  data.header = xrd.readHeader.ASC(fname)
  expect_equal(nrow(data.header), 74)

  expect_equal(data.header$val[grep('scan mode', data.header$name)],"  2theta/theta")
})
