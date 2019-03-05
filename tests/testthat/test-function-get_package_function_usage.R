test_that("get_package_function_usage() works", {

  scripts <- dir(
    system.file("extdata", package = "kwb.fakin"),
    pattern = "^testcalls",
    full.names = TRUE
  )

  get_usage <- function(script) {
    tree <- kwb.code::parse_scripts(dirname(script), basename(script))
    kwb.fakin:::get_package_function_usage(tree, package = "kwb.code")
  }

  # Unfortunateley the package (kwb.code) needs to be on the search path

  usage <- lapply(scripts, get_usage)

  expect_identical(usage[[1]]$name, c("get_full_function_info", "parse_scripts"))
  expect_identical(usage[[2]]$name, c("parse_scripts"))
  expect_identical(usage[[2]]$explicit, 0)
  expect_identical(usage[[2]]$implicit, 1)
  expect_identical(usage[[3]]$name, c("parse_scripts"))
  expect_identical(usage[[3]]$explicit, 1)
  expect_identical(usage[[3]]$implicit, 0)
})
