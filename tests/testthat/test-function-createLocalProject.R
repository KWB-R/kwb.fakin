test_that("createLocalProject() works", {

  f <- kwb.fakin:::createLocalProject

  old_opts <- options(kwb.fakin.paths = list(projects = tempdir()))
  on.exit(options(old_opts))

  fs::dir_create(recurse = TRUE, file.path(tempdir(), c(
    "AUFTRAEGE/_Angebote_in_Arbeit/a1",
    "AUFTRAEGE/_Auftraege_laufend/a2",
    "SUW_Department/Projects/b",
    "GROUNDWATER/PROJECTS/c",
    "WWT_Department/PROJECTS/d"
  )))

  # Shortcut
  capture <- capture.output

  expect_error(f())
  expect_error(capture(f("x")))

  capture(f("a1"))
  capture(f("a2"))
  capture(f("b"))

  targetdir <- file.path(kwb.utils::get_homedir(), "Documents", "Projekte")

  expect_true(all(c("a1", "a2", "b") %in% dir(targetdir)))

  fs::dir_delete(file.path(targetdir, c("a1", "a2", "b")))
})
