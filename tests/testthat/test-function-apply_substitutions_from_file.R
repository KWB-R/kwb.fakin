test_that("apply_substitutions_from_file() works", {

  files <- dir(kwb.fakin:::extdata_file("config"), "^rep", full.names = TRUE)

  f <- kwb.fakin:::apply_substitutions_from_file

  expect_error(f())

  expect_identical(f("a", file = files[1]), "a")
})
