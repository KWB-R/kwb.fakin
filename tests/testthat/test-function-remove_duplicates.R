test_that("remove_duplicates() works", {
  x <- c("a", "b", "c")
  expect_identical(remove_duplicates(x), x)
  expect_identical(remove_duplicates(c(x, "a")), x)
  expect_identical(remove_duplicates(c(x, "a", "b", "c")), x)
  expect_identical(remove_duplicates(c(x, "a", "b", "c", "d")), c(x, "d"))
})
