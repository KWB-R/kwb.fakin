test_that("remove_empty() works", {

  f <- kwb.fakin:::remove_empty

  expect_error(f())

  expect_identical(f(c("a", "", "b")), c("a", "b"))
  expect_identical(f(c("a", "b", "c")), c("a", "b", "c"))
})
