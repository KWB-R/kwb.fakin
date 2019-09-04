test_that("subtree_for_treejack() works", {

  f <- kwb.fakin:::subtree_for_treejack

  expect_error(f())

  expect_error(f(system.file(package = "kwb.fakin")))

  f("root", paths = c("root/a/b", "root/a/c"), stdout = TRUE)
  f("root", paths = c("root/a/b", "root/a/c"), stdout = FALSE)
})
