test_that("github_package_files() works", {

  f <- kwb.fakin:::github_package_files

  expect_error(f())
  expect_error(f("repo", "path"))
  expect_error(f("kwb-r/kwb.utils", "no such file"))

  repo <- "kwb-r/kwb.utils"
  expect_identical(f(repo, "DESCRIPTION"), "DESCRIPTION")
  expect_true(all(grepl("\\.R$", f(repo, "R"))))
})
