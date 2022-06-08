test_that("extract_properties() works", {

  f <- kwb.fakin:::extract_properties

  expect_error(f())

  x <- c("abc", "def")
  patterns <- c("a", "e")
  replacements <- c("ananas", "egon")

  result <- f(x, patterns, replacements)

  expect_identical(result, c(abc = "ananas", def = "egon"))

  result <- f(x, patterns, replacements, as_data_frame = TRUE)

  expect_identical(names(result), c("name", "ananas", "egon"))
})
