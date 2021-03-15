test_that("get_file_duplicates() works", {

  f <- kwb.fakin:::get_file_duplicates

  expect_error(f())

  capture.output(result <- f(c("folder1/c", "folder2/c"), pattern = "c$"))

  expect_is(result, "list")
  expect_identical(names(result), "c")
  expect_identical(result$c, c("folder1/c", "folder2/c"))
})
