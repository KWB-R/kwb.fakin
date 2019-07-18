test_that("args_treemap() works", {

  testthat::expect_identical(
    names(kwb.fakin:::args_treemap()),
    c("index", "type", "border.col", "vSize", "vColor", "title", "title.legend")
  )

})
