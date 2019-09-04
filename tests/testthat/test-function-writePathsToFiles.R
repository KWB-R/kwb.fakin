test_that("writePathsToFiles() works", {

  f <- kwb.fakin:::writePathsToFiles

  expect_error(f())

  xall <- list(
    c("a", "b", "c"),
    c("d", "e", "f")
  )

  file <- tempfile(fileext = ".txt")

  f(xall, file = file)
})
