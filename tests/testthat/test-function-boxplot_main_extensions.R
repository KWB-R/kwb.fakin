test_that("boxplot_main_extensions() works", {

  f <- kwb.fakin:::boxplot_main_extensions

  expect_error(f())

  f(data.frame(
    extension = c("docx", "xls", "docx"),
    size = c(1, 2, 3)
  ))
})
