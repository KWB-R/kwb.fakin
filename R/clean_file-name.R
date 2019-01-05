if (FALSE)
{
  #paths <- readLines(file, encoding = "ISO-8859-1")
  paths <- readLines(file, encoding = "latin1")
  catLines(paths[1:100])

  table(Encoding(paths))

  # basename() removes the information on the Encoding!
  table(Encoding(file_names_raw <- basename(paths)))

  file_names <- file_names_raw

  # Neglect files starting or ending with "~" (backup files?)
  file_names <- file_names[! grepl("^~|~$|~\\d+[.]PDF$|[.]~FL$", file_names)]

  file_names <- get_non_standard_names(file_names)
  length(file_names)
  head(file_names, 100)

  file_names <- clean_file_name(file_names)

  x <- file_names[20]

  get_special_character_info(file_names[17], bytes_per_char = 2)

  # Use restoreEncoding to keep the declared Encodings
  table(Encoding(file_names_old <- restoreEncoding(paths, basename)))

  table(name_is_ok(file_names_old))

  file_names <- file_names_old

  length(file_names)

  kwb.utils::catLines(sort_unique(file_names[non_standard]))

  file_names <- standardise_file_names(file_names)
}

# get_non_standard_names -------------------------------------------------------
get_non_standard_names <- function(x)
{
  x[! name_is_ok(x)]
}

# name_is_ok -------------------------------------------------------------------

#' Is the Name Ok According to Our Best Practices?
#'
#' @param x vector of character
#' @param mildness level of mildness. 1: not mild, all characters must be
#'   hyphen or alphanumeric or dot or underscore, 2: more mild, all characters
#'   must be one of the above or space
#' @export
#'
#' @return vector of logical as long as \code{x}
#'
#' @examples
#' name_is_ok(c("a", "$", ".", " "))
#' name_is_ok(c("a", "$", ".", " "), mildness = 2)
#'
name_is_ok <- function(x, mildness = 1)
{
  stopifnot(mildness %in% 1:2)

  patterns <- list(
    "^[-A-Za-z0-9._]+$",
    "^[-A-Za-z0-9._ ]+$"
  )

  grepl(patterns[[mildness]], x)
}

# clean_file_name --------------------------------------------------------------
clean_file_name <- function(x)
{
  x <- gsub("\xc2\xa0", " ", x)
  x <- gsub("\xc2\xa6", "|", x)
  x <- gsub("\xc2\xa8", "\"", x)
  x <- gsub("\xc2\xab", "<<", x)
  x <- gsub("\xc2\xac", "-", x)
  x <- gsub("\xc2\xad", "_", x)
  x <- gsub("\xc2\xae", "_registered_", x)
  x <- gsub("\xc2\xb3", "3", x)
  x <- gsub("\xc2\xb4", "'", x)
  x <- gsub("\xc2\xb5", "_micro_", x)
  x <- gsub("\xc2\xba", "o", x)
  x <- gsub("\xc2\xbb", ">>", x)
  x <- gsub("\xc3\x80", "A", x)
  x <- gsub("\xc3\x81", "A", x)
  x <- gsub("\xc3\x82", "A", x)
  x <- gsub("\xc3\x83", "A", x)
  x <- gsub("\xc3\x84", "Ae", x)
  x <- gsub("\xc3\x85", "A", x)
  x <- gsub("\xc3\x8d", "I", x)
  x <- gsub("\xc3\x96", "Oe", x)
  x <- gsub("\xc3\x98", "O", x)
  x <- gsub("\xc3\x9c", "Ue", x)
  x <- gsub("\xc3\x9d", "Y", x)
  x <- gsub("\xc3\x9f", "ss", x)
  x <- gsub("\xc3\xa0", "a", x)
  x <- gsub("\xc3\xa1", "a", x)
  x <- gsub("\xc3\xa2", "a", x)
  x <- gsub("\xc3\xa4", "ae", x)
  x <- gsub("\xc3\xa5", "a", x)
  x <- gsub("\xc3\xa6", "ae", x)
  x <- gsub("\xc3\xa8", "e", x)
  x <- gsub("\xc3\xa7", "c", x)
  x <- gsub("\xc3\xa9", "e", x)
  x <- gsub("\xc3\xaa", "e", x)
  x <- gsub("\xc3\xab", "e", x)
  x <- gsub("\xc3\xad", "i", x)
  x <- gsub("\xc3\xb1", "n", x)
  x <- gsub("\xc3\xb2", "o", x)
  x <- gsub("\xc3\xb3", "o", x)
  x <- gsub("\xc3\xb4", "o", x)
  x <- gsub("\xc3\xb6", "oe", x)
  x <- gsub("\xc3\xb8", "o", x)
  x <- gsub("\xc3\xbb", "u", x)
  x <- gsub("\xc3\xbc", "ue", x)
  x <- gsub("\xc3\xbd", "y", x)
  x <- gsub("\\((\\d+)\\)", "_v\\1", x)
  x <- gsub("\\[(\\d+)\\]", "_v\\1", x)
  x <- gsub("n\xc2\xb0", "number", x)
  x <- gsub("\xc2\xb0", "_degree_", x)
  x <- gsub("[+&]", "_and_", x)
  x <- gsub("=", "_equal_", x)
  x <- gsub(",", "_comma_", x)
  x <- gsub(";", "_semicolon_", x)
  x <- gsub("'", "_apostrophe_", x)
  x <- gsub("!", "_exclamation_", x)
  x <- gsub("%", "_percent_", x)
  x <- gsub("#", "_hash_", x)
  x <- gsub("@", "_at_", x)
  x <- gsub("a\\^", "a", x)
  x <- gsub("([aou])\"", "\\1e", x) # a" <- ae, o" = oe, u" = ue
  x <- gsub("([aou])\\|e", "\\1e", x) # a" <- ae, o" = oe, u" = ue
  x <- gsub(" ", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("[(){}]", "", x)
  x <- gsub("<<|>>", "", x)
  x <- gsub("\\[|\\]", "", x)
  x <- gsub("_-_", "_", x)

  x <- gsub("\u0084", "", x)
  x <- gsub("\u0092", "_apostrophe_", x)
  x <- gsub("\u0096", "", x)

  x
}

# standardise_file_names -------------------------------------------------------
standardise_file_names <- function(x)
{
  kwb.utils::multiSubstitute(x, list(
    "\xe4" = "ae",
    "\xdc" = "Ue",
    "[ -]+" = "-"
  ))
}
