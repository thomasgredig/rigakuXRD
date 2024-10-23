test_that("xrd.import: Load XRD sample file data", {
  sel.Files=c(1,3)
  file.list = xrd.getSampleFiles()[sel.Files]
  nLen=c()
  for(f in file.list) {
    d = xrd.import(f)
    nLen=c(nLen,nrow(d))
  }
  expect_equal(nLen,c(7601,1401,2201)[sel.Files])
})


test_that("xrd.import: check for uniformity of data columns", {
  sel.Files=c(1,3,4)
  file.list = xrd.getSampleFiles()[sel.Files]
  nLen=c()
  for(f in file.list) {
    d = xrd.import(f)
    #print(names(d))
    nLen=c(nLen,length(names(d)))
  }
  expect_equal(nLen,rep(4,length(sel.Files)))
})



test_that("Load XRD sample file data", {
  fname = xrd.getSampleFiles()[1]
  expect_true(grepl('asc$',fname))

  data.header = xrd.readHeader.ASC(fname)
  expect_equal(nrow(data.header), 74)

  expect_equal(data.header$val[grep('scan mode', data.header$name)],"  2theta/theta")
})



test_that("Test files have data.", {
  file.list = xrd.getSampleFiles()
  for(fname in file.list) {
    d = xrd.import(fname)
    expect_true(nrow(d)>1000)
  }
})

test_that("xrd.import: invalid file", {
  xrd_filename = tempfile()
  expect_warning(xrd.import(xrd_filename))
})


