test_that("createLocalProject() works", {

  f <- kwb.fakin:::createLocalProject

  old_opts <- options(kwb.fakin.paths = list(projects = tempdir()))
  on.exit(options(old_opts))

  fs::dir_create(recurse = TRUE, file.path(tempdir(), c(
    "Auftraege/a",
    "SUW_Department/Projects/b",
    "GROUNDWATER/PROJECTS/c",
    "WWT_Department/PROJECTS/d"
  )))

  expect_error(f())
  expect_error(f("x"))

  f("a")
  f("b")

  targetdir <- file.path(kwb.utils::get_homedir(), "Documents", "Projekte")

  expect_true(all(c("a", "b") %in% dir(targetdir)))

  fs::dir_delete(file.path(targetdir, c("a", "b")))
})
