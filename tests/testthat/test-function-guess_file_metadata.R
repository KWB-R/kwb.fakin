test_that("guess_file_metadata() works", {

  f <- kwb.fakin:::guess_file_metadata

  expect_error(f())

  file1 <- tempfile()
  file2 <- tempfile()

  writeLines("a,b,c", file1)

  writeLines(con = file2, c(
    "path,size,user",
    "a/b/c,0,hauke"
  ))

  result1 <- f(file1)
  result2 <- f(file2)

  columns <- c("paths", "forbidden", "header", "windows", "sep", "ncol")

  expect_identical(names(result1), columns)
  expect_identical(names(result2), columns)

  expect_identical(result1$sep, ",")
  expect_identical(result2$sep, ",")
})
