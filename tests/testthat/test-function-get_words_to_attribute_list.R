test_that("get_words_to_attribute_list() works", {

  f <- kwb.fakin:::get_words_to_attribute_list

  expect_error(f())

  file <- kwb.fakin:::extdata_file("config/words-to-attributes.csv")

  f(file)
})
