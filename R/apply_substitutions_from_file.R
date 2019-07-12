# apply_substitutions_from_file ------------------------------------------------
apply_substitutions_from_file <- function(x, file, dbg = TRUE)
{
  y <- kwb.utils::multiSubstitute(x, read_substitutions_from_file(file))

  cat_changes_if(dbg, x, y)

  y
}

# read_substitutions_from_file -------------------------------------------------
read_substitutions_from_file <- function(file)
{
  substitution_data <- read_csv_de(file)

  kwb.utils::toLookupList(data = substitution_data)
}
