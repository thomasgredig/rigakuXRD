test_that("export data to genx", {
  filename <- xrd.getSampleFiles('txt')
  d <- xrd.import(filename)
  genx_file = export2genx(d, tempdir())
  expect_equal(file.info(genx_file)$size, 3385)
  lines <- readLines(genx_file)
  expect_equal(length(lines), 150)
})
