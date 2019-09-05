test_that("check_or_set_ending_slash() works", {

  f <- kwb.fakin:::check_or_set_ending_slash

  expect_error(f())

  expect_true(all(grepl("/$", f(c("a", "b/")))))
})
