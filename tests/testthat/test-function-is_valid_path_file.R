test_that("is_valid_path_file() works", {

  f <- kwb.fakin:::is_valid_path_file

  expect_error(f())

  f(structure(
    list(paths = TRUE, forbidden = TRUE),
    file = "my-file",
    first_rows = c("row-1", "row-2")
  ))
})
