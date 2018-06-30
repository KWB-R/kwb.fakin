test_that("get_store_folder() works", {

  path <- try(kwb.fakin:::get_store_folder())

  if (! inherits(path, "try-error")) {

    expect_true(file.exists(path))
  }
})
