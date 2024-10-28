test_that("export data to genx", {
  filename <- xrd.getSampleFiles('txt')
  d <- xrd.import(filename)
  genx_file = export2genx(d, tempdir())
  expect_true(file.info(genx_file)$size >= 3385)  # on windows extra 150 chars
  lines <- readLines(genx_file)
  expect_equal(length(lines), 151)
})
