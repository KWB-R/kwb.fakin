# get_non_standard_names -------------------------------------------------------
get_non_standard_names <- function(x)
{
  x[! fakin.path.app::name_is_ok(x)]
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
