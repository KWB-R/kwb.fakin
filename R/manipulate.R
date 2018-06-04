# cut_left ---------------------------------------------------------------------
cut_left <- function(x, start_string)
{
  string_length <- nchar(start_string)

  matching <- substr(x, 1, string_length) == start_string

  if (! all(matching)) {

    stop(
      sum(! matching), " strings in x do not start with '", start_string, "'!"
    )
  }

  substr(x, string_length + 1, nchar(x))
}
