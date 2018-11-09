# get_file_info_powershell -----------------------------------------------------
get_file_info_powershell <- function(root_dir, top_n = 10)
{
  properties <- get_file_property_names(groups = 1)

  file_info_text <- run_windows_search_query(sql = sprintf(
    "SELECT %s%s FROM SYSTEMINDEX WHERE SCOPE='file:%s'",
    if (! is.null(top_n)) sprintf("TOP %d ", top_n) else "",
    kwb.utils::commaCollapsed(properties),
    kwb.utils::windowsPath(root_dir)
  ))

  read.table(
    text = file_info_text, sep = ",", header = TRUE, stringsAsFactors = FALSE
  )
}

# get_file_property_names ------------------------------------------------------
get_file_property_names <- function(groups = 1)
{
  property_info <- read.table(
    file = system.file("extdata", "property_names.txt", package = "kwb.fakin"),
    sep = ",", header = TRUE, stringsAsFactors = FALSE
  )

  property_info$property[property_info$group %in% groups]
}

# run_windows_search_query -----------------------------------------------------
run_windows_search_query <- function(sql)
{
  file <- "powershell_template_search_query.txt"

  file <- system.file("extdata", file, package = "kwb.fakin")

  template <- kwb.utils::collapsed(readLines(file), "\n")

  sql_definition <- to_multirow_assignment(sql)

  script_text <- kwb.utils::resolve(template, sql_definition = sql_definition)

  run_powershell(script_text)
}

# to_multirow_assignment -------------------------------------------------------
to_multirow_assignment <- function(x, variable_name = deparse(substitute(x)))
{
  parts <- split_string_into_parts(x, 70)

  first_assignment <- sprintf("$%s = \"\"", variable_name)

  next_assignments <- sprintf("$%s += \"%s\"", variable_name, parts)

  kwb.utils::collapsed(c(first_assignment, next_assignments), "\n")
}

# split_string_into_parts ------------------------------------------------------
split_string_into_parts <- function(x, size = 80)
{
  n <- nchar(x)

  if (n > size) {

    starts <- size * (seq_len((n - 1) %/% size + 1) - 1)

    bounds <- kwb.utils::startsToRanges(starts, n, 1, 0)

    apply(bounds, MARGIN = 1, function(ii) substr(x, ii[1], ii[2]))

  } else {

    x
  }
}

# run_powershell ---------------------------------------------------------------
run_powershell <- function(script_text)
{
  temp_dir <- file.path(tempdir(), "powershell")

  temp_dir <- kwb.utils::createDirectory(temp_dir, dbg = FALSE)

  script_file <- file.path(temp_dir, "search_query.ps1")

  writeLines(script_text, script_file)

  command <- sprintf("powershell -File \"%s\"", script_file)

  system(command, intern = TRUE)
}

# property_is_available --------------------------------------------------------
property_is_available <- function(property_names)
{
  sqls <- sprintf("SELECT TOP 1 %s FROM SYSTEMINDEX", property_names)

  search_results <- lapply(seq_along(sqls), function(i) {
    kwb.utils::catAndRun(
      sprintf(
        "Checking property '%s' (%d/%d)",
        property_names[i], i, length(property_names)
      ),
      run_windows_search_query(sqls[i])
    )
  })

  ! sapply(search_results, function(x) any(grepl("Ausnahme", x)))
}
