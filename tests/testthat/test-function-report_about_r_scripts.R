testthat::skip_on_appveyor()

test_that("report_about_r_scripts() works", {

  f <- kwb.fakin:::report_about_r_scripts

  expect_error(f())

  f(root = system.file("extdata", package = "kwb.fakin"), show = FALSE)
})
