# guess_expected_encoding ------------------------------------------------------
guess_expected_encoding <- function(file, expected, dbg = TRUE)
{
  encodings <- readr::guess_encoding(file)$encoding

  kwb.utils::printIf(dbg, encodings, "Encodings guessed by readr")

  common <- intersect(tolower(encodings), tolower(expected))

  encoding <- if (length(common)) {
    common[1]
  } else {
    expected[1]
  }

  kwb.utils::catIf(dbg, sprintf("Selected encoding: %s\n", encoding))
  encoding
}
