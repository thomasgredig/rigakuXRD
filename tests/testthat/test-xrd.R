test_that("xrd.import: Load XRD sample file data", {
  sel.Files=c(1,3)
  file.list = xrd_sample_files()[sel.Files]
  nLen=c()
  for(f in file.list) {
    d = xrd_import(f)
    nLen=c(nLen,nrow(d))
  }
  expect_equal(nLen,c(7601,1401,2201)[sel.Files])
})


test_that("xrd.import: check for uniformity of data columns", {
  sel.Files=c(1,3,4)
  file.list = xrd_sample_files()[sel.Files]
  nLen=c()
  for(f in file.list) {
    d = xrd_import(f)
    # print(names(d))
    nLen=c(nLen,length(names(d)))
  }
  expect_equal(nLen,rep(4,length(sel.Files)))
})



test_that("Load XRD sample file data", {
  fname = xrd_sample_files('asc')
  expect_true(grepl('asc$',fname))

  data.header = xrd.readHeader.ASC(fname)
  expect_equal(nrow(data.header), 74)

  expect_equal(data.header$val[grep('scan mode', data.header$name)],"  2theta/theta")
})



test_that("Test files have data.", {
  file.list = xrd_sample_files()
  for(fname in file.list) {
    d = xrd_import(fname)
    expect_true(nrow(d)>1000)
  }
})

test_that("xrd_import: invalid file or invalid data", {
  # test importing a file that does not exist
  xrd_filename = tempfile(fileext = ".abc")
  expect_error(xrd_import(xrd_filename))

  # test importing a file with unrecognized file extension
  writeLines("not an XRD data file", xrd_filename)
  expect_warning(xrd_import(xrd_filename))

  # test importing a file with recognized file extension, but incorrect data
  xrd_filename = tempfile(fileext = ".txt")
  writeLines("not an XRD data file, but TXT extension", xrd_filename)
  expect_error(xrd_import(xrd_filename))
})


