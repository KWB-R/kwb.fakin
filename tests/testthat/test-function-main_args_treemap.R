test_that("main_args_treemap() works", {

  expect_identical(
    names(kwb.fakin:::main_args_treemap()),
    c("index", "type", "border.col")
  )

})

