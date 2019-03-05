test_that("get_package_function_usage() works", {

  scripts <- dir(
    system.file("extdata", package = "kwb.fakin"),
    pattern = "^testcalls",
    full.names = TRUE
  )

  get_usage <- function(script) {
    tree <- kwb.code::parse_scripts(dirname(script), basename(script))
    kwb.fakin:::get_package_function_usage(tree, package = "kwb.logger")
  }

  usage <- lapply(scripts, get_usage)

  identical(usage[[1]]$name, c("read_aquatroll_data", "read_logger_LT_EDGE_M100"))
  identical(usage[[2]]$name, c("read_aquatroll_data"))
  identical(usage[[3]]$name, c("read_aquatroll_data"))
})
