test_that("to_tree() works", {

  to_tree <- kwb.fakin:::to_tree

  base_test <- function(tree, first_level) {
    expect_is(tree, "list")
    expect_is(tree, "path_tree")
    expect_identical(names(tree), first_level)
  }

  expect_error(to_tree())

  tree <- to_tree("a")
  base_test(tree, first_level = "a")
  expect_identical(names(tree), "a")
  expect_identical(tree$a, "")

  tree <- to_tree(c("a/b/c", "a/b/d", "a/e", "f/g"))
  base_test(tree, first_level = c("a", "f"))
  expect_identical(sort(names(tree$a)), c("b", "e"))
  expect_identical(names(tree$f), "g")
  expect_identical(names(tree$a$b), c("c", "d"))

  to_tree(c("a", "a"))
})
