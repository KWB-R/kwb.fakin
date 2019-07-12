# report_about_r_scripts -------------------------------------------------------

#' Create a HTML Report About R Scripts
#'
#' Create a HTML Report about each R script below a root directory. The report
#' will contain an overview plot showing the number or rows for each script. In
#' addition, one plot is generated per script, showing for each function
#' defined in the script, the number of expressions contained in the function.
#'
#' @param root path to directory from which to start looking for R scripts
#' @param scripts optional. Paths to R scripts, relative to the path given in
#'   \code{root}
#' @param show if \code{TRUE} the created HTML file is opened in your default
#'   browser
#'
#' @return path to the created HTML file, invisibly
#'
#' @export
#'
report_about_r_scripts <- function(
  root, scripts = dir(root, "\\.R$", ignore.case = TRUE, recursive = TRUE),
  show = TRUE
)
{
  #kwb.utils::assignArgumentDefaults(kwb.fakin::report_about_r_scripts)
  #kwb.utils::assignPackageObjects("kwb.fakin")

  trees <- kwb.code::parse_scripts(root = root, scripts = scripts)

  script_info <- kwb.code::to_full_script_info(trees)

  all_function_info <- kwb.code::get_full_function_info(trees)

  rmd_source <- c(
    rmd_intro("R Script Analysis"),
    code_to_r_block("plot_row_numbers(params$script_info)"),
    "",
    "# Overview of Scripts and Functions",
    "",
    get_rmd_script_function_enumeration(all_function_info),
    "",
    "# Functions per Script",
    "",
    get_rmd_per_script(all_function_info, scripts = names(trees))
  )

  invisible(render_text(rmd_source, show = show, params_to_rmd = list(
    script_info = script_info,
    all_function_info = all_function_info
  )))
}

# rmd_intro --------------------------------------------------------------------
rmd_intro <- function(title, author = "Hauke Sonnenberg")
{
  double_quoted <- function(x) kwb.utils::hsQuoteChr(x)

  c(
    "---",
    paste("title:", double_quoted(title)),
    paste("author:", double_quoted(author)),
    paste("date:", double_quoted(Sys.Date())),
    paste("output:", "html_document"),
    "params:",
    "  script_info: NULL",
    "  all_function_info: NULL",
    "---\n"
  )
}

# code_to_r_block --------------------------------------------------------------
code_to_r_block <- function(code_lines, echo = FALSE)
{
  c(
    sprintf("```{r echo = %s}", echo),
    code_lines,
    "```"
  )
}

# get_rmd_per_script -----------------------------------------------------------
get_rmd_per_script <- function(all_function_info, scripts)
{
  rmd_text <- character(0)

  for (script in scripts) {

    #script <- scripts[1]

    rmd_text <- c(rmd_text, sprintf(
      "## %s {#%s}\n\n", script, path_to_acronym(script)
    ))

    belongs_to_script <- all_function_info$script == script

    rmd_new <- if (any(belongs_to_script)) {

      c(
        get_rmd_function_enumeration(all_function_info, script),
        "",
        "These functions contain the following numbers of expressions:",
        "",
        code_to_r_block(sprintf(
          "plot_expression_numbers(params$all_function_info, \"%s\")", script
        ))
      )

    } else {

      "There are no functions defined in this script."
    }

    rmd_text <- c(rmd_text, rmd_new, "")
  }

  rmd_text
}

# get_rmd_script_function_enumeration ------------------------------------------
get_rmd_script_function_enumeration <- function(all_function_info)
{
  function_info_list <- split(all_function_info, all_function_info$script)

  unlist(lapply(names(function_info_list), function(script) {
    c(
      paste(sprintf("* [%s](#%s)", script, path_to_acronym(script))),
      paste(sprintf("    + %s()", function_info_list[[script]]$functionName))
    )
  }))
}

# path_to_acronym --------------------------------------------------------------
path_to_acronym <- function(x)
{
  x <- gsub("[/ _.]", "-", tolower(x))
  x <- gsub("^-+", "", x)
  x
}

# get_rmd_function_enumeration -------------------------------------------------
get_rmd_function_enumeration <- function(all_function_info, script)
{
  function_info <- filter_function_info_for_script(all_function_info, script)

  c(
    sprintf("The script `%s` defines the following functions:", script),
    "",
    paste(sprintf("* %s()", function_info$functionName))
  )
}

# render_text ------------------------------------------------------------------
render_text <- function(
  rmd_source, params_to_rmd, rmd_file = tempfile(fileext = ".Rmd"), show = TRUE
)
{
  writeLines(rmd_source, con = rmd_file)

  kwb.utils::catAndRun(sprintf("Rendering '%s'", rmd_file), {

    html_file <- rmarkdown::render(
      rmd_file, params = params_to_rmd, quiet = TRUE
    )
  })

  if (isTRUE(show)) {

    utils::browseURL(html_file)
  }

  html_file
}

# plot_row_numbers -------------------------------------------------------------
plot_row_numbers <- function(script_info)
{
  plot_horizontal_bars(
    data = script_info, column_values = "rows", column_labels = "script",
    xlab = "Number of Lines in Script"
  )
}

# plot_expression_numbers ------------------------------------------------------
plot_expression_numbers <- function(all_function_info, script)
{
  function_info <- filter_function_info_for_script(all_function_info, script)

  plot_horizontal_bars(
    data = function_info,
    column_values = "n.expr",
    column_labels = "functionName",
    xlab = "Number of Expressions in Function"
  )
}

# filter_function_info_for_script ----------------------------------------------
filter_function_info_for_script <- function(all_function_info, script)
{
  # Get the vector of script names
  scripts <- kwb.utils::selectColumns(all_function_info, "script")

  function_info <- all_function_info[scripts == script, ]

  # Get the vector of function names
  function_names <- kwb.utils::selectColumns(function_info, "functionName")

  # Order rows by function name
  function_info[order(function_names), ]
}

# plot_horizontal_bars ---------------------------------------------------------
plot_horizontal_bars <- function(data, column_values, column_labels, xlab = "")
{
  values <- kwb.utils::selectColumns(data, column_values)

  text_labels <- kwb.utils::selectColumns(data, column_labels)

  n_digits <- nchar(as.character(max(values))) - 1

  x_max <- 1.2 * round(max(values), - n_digits)

  x_shift <- 0.03 * x_max

  y <- graphics::barplot(
    xlim = c(-x_max, x_max), values, horiz = TRUE, las = 1, xlab = xlab,
    axes = FALSE
  )

  graphics::axis(1, at = seq(0, x_max, by = 10^n_digits))

  graphics::text(-x_shift, y, text_labels, adj = 1, cex = 0.8)

  graphics::text(values + x_shift, y, values, cex = 0.8, adj = 0)
}
