test_that("write_batch_list_files() works", {

  f <- kwb.fakin:::write_batch_list_files

  expect_error(f())

  output_file <- "output.txt"

  capture.output(f(
    root = system.file(package = "kwb.fakin"), output_file = output_file
  ))

  batchlines <- readLines(file.path(tempdir(), "list_files.bat"))

  expect_true(any(grepl(output_file, batchlines)))
})
