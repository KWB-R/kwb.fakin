test_that("read_path_information() works", {

  f <- kwb.fakin:::read_path_information

  expect_error(f())

  expect_message(f(file_info_dir = system.file(package = "kwb.fakin")))

  f(kwb.fakin:::extdata_file(""), pattern = "^example_file_info")
})
