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
  trees <- kwb.code::parse_scripts(root = root, scripts = scripts)

  script_info <- kwb.code::to_full_script_info(trees)

  all_function_info <- kwb.code::get_full_function_info(trees)

  rmd_source <- c(
    rmd_intro("R Script Analysis"),
    code_to_r_block("plot_row_numbers(params$script_info)"),
    "",
    "# Functions per Script",
    ""
    ,
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

    rmd_text <- c(rmd_text, sprintf("## %s\n", script))

    belongs_to_script <- all_function_info$script == script

    rmd_new <- if (any(belongs_to_script)) {

      code_to_r_block(sprintf(
        "plot_expression_numbers(params$all_function_info, \"%s\")", script
      ))

    } else {

      "There are no functions defined in this script."
    }

    rmd_text <- c(rmd_text, rmd_new, "")
  }

  rmd_text
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
    xlab = "Number of Rows"
  )
}

# plot_expression_numbers ------------------------------------------------------
plot_expression_numbers <- function(all_function_info, script)
{
  function_info <- all_function_info[all_function_info$script == script, ]

  plot_horizontal_bars(
    data = function_info,
    column_values = "n.expr",
    column_labels = "functionName",
    xlab = "Number of Expressions in Function"
  )
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

