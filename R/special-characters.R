# get_special_character_info ---------------------------------------------------

#' Get Special Characters and Their Byte Codes
#'
#' @param text vector of character of length one
#' @param context_length number of characters left and right of special
#'   character to be put into column \code{context}
#' @param bytes_per_char number of bytes per character
#'
#' @return data frame with columns \code{special} (special characters) and
#'   \code{bytes} (hexadecimanl byte codes as a space separated string),
#'   \code{context} (strings "around" the special characters)
#'
#' @export
#'
#' @examples
#' (text <- kwb.fakin:::example_string_with_specials("de"))
#'
#' get_special_character_info(text)
#'
get_special_character_info <- function(
  text, context_length = 7, bytes_per_char = 2
)
{
  # kwb.utils::assignArgumentDefaults(get_special_character_info)

  stopifnot(length(text) == 1)

  characters <- strsplit(text, "", useBytes = TRUE)[[1]]

  is_special <- ! isASCII(characters)

  special_characters <- characters[is_special]

  bytes <- lapply(special_characters, charToRaw)

  indices <- which(is_special)

  indices <- indices[seq(1, length(indices), by = bytes_per_char)]

  collapsed_0 <- function(x) kwb.utils::collapsed(x, "")

  contexts <- sapply(indices, function(i) {
    j <- i + bytes_per_char - 1
    min_index <- max(i - context_length, 1)
    max_index <- min(j + context_length, length(is_special))
    paste0(
      collapsed_0(characters[min_index:(i - 1)]),
      paste0(" [ ", collapsed_0(characters[i:j]), " ] "),
      collapsed_0(characters[(j + 1):max_index])
    )
  })

  kwb.utils::noFactorDataFrame(
    special = special_characters,
    bytes = sapply(bytes, collapsed_0),
    context = contexts
  )
}

# example_string_with_specials -------------------------------------------------
example_string_with_specials <- function(language_code)
{
  if (language_code == "de") {

    "Sch\xc3\xb6ne Gr\xc3\xbc\xc3\x9fe"

  } else {

    stop_("So far, only implemented for language_code = 'de'")
  }
}

# replaceSpecial ---------------------------------------------------------------
replaceSpecial <- function(x, all = TRUE, dbg = TRUE) {

  if (all) {

    kwb.utils::catAndRun(
      paste("Replacing special characters in", length(x), "strings"),
      expr = kwb.utils::multiSubstitute(x, replacements(), useBytes = TRUE),
      dbg = dbg
    )

  } else {

    x <- kwb.utils::catAndRun(
      paste("Replacing 2 Byte chars in", length(x), "strings"),
      kwb.utils::multiSubstitute(x, replacements2(), useBytes = TRUE),
      dbg = dbg
    )

    x <- kwb.utils::catAndRun(
      paste("Replacing 1 Byte chars in", length(x), "strings"),
      kwb.utils::multiSubstitute(x, replacements1()),
      dbg = dbg
    )
  }

  x
}

# replacements -----------------------------------------------------------------
replacements <- function()
{
  list(
    "a\xa6\xea" = "ae",
    "o\xa6\xea" = "oe",

    "a\xa8" = "ae",
    "u\xa8" = "ue",
    "o\xa8" = "oe",

    "[+]\x83" = "ss",
    "[+]\xa3" = "Ue",
    "[+]\xba" = "<+xba>",
    "[+]\xc2" = "oe",

    "[-]\xc0" = "ss",

    "\x83\xc2" = "<x83xc2>",
    "\x9a\xed" = "<x9axed>",

    "\xa6\xe9" = "<xa6xe9>",

    "\xf6\xdf" = "oess",
    "\xfc\xdf" = "uess",

    "\x80" = "<x80>",
    "\x84" = "<x84>",
    "\x85" = "<x85>",
    "\x8a" = "<x8a>",
    "\x8e" = "<x8e>",

    "\x92" = "_",
    "\x93" = "<x93>",
    "\x96" = "<x96>",
    "\x97" = "_",

    "\xa0" = "<xa0>",
    "\xa4" = "ae",
    "\xa6" = "<xa6>",
    "\xa7" = "_Prgrph_",
    "\xab" = "_",
    "\xac" = "<xac>",
    "\xad" = "<xad>",
    "\xae" = "_",

    "\xb0" = "<xb0>",
    "\xb3" = "<xb3>",
    "\xb4" = "_",
    "\xb5" = "<xb5>",
    "\xba" = "<xba>",
    "\xbb" = "_",

    "\xc0" = "a",
    "\xc1" = "<xc1>",
    "\xc3" = "ae",
    "\xc4" = "Ae",
    "\xc5" = "<xc5>",
    "\xcd" = "<xcd>",

    "\xd1" = "ae",
    "\xd6" = "Oe",
    "\xd8" = "OE",
    "\xdc" = "Ue",
    "\xdd" = "<xdd>",
    "\xdf" = "ss",

    "\xe0" = "<xe0>",
    "\xe1" = "<xe1>",
    "\xe2" = "<xe2>",
    "\xe4" = "ae",
    "\xe5" = "<xe5>",
    "\xe6" = "<xe6>",
    "\xe7" = "c",
    "\xe8" = "e",
    "\xe9" = "e",
    "\xea" = "<xea>",
    "\xeb" = "<xeb>",
    "\xed" = "<xed>",

    "\xf1" = "<xf1>",
    "\xf2" = "o",
    "\xf3" = "o",
    "\xf4" = "o",
    "\xf6" = "oe",
    "\xf7" = "oe",
    "\xf8" = "<xf8>",
    "\xfb" = "<xfb>",
    "\xfc" = "ue",
    "\xfd" = "<xfd>"
  )
}

# replacements2 ----------------------------------------------------------------
replacements2 <- function() list(
  "[+]\xa3" = "Ue",
  "[+]\\+" = "ue",
  "[+]\xba" = "??",
  "[+]\xc2" = "oe",
  "[-]\xc0" = "ss",
  "\x84\xbc" = "??",
  "\x8c\x86" = "??",
  "\x99\x83" = "??",
  "\xa0\xbc" = "??",
  "\xfc\xbe" = "??"
)

# replacements1 ----------------------------------------------------------------
replacements1 <- function() list(
  "\xe4" = "ae",
  "\xc4" = "Ae",
  "\xf6" = "oe",
  "\xd6" = "Oe",
  "\xfc" = "ue",
  "\xdc" = "Ue",
  "\xdf" = "ss",
  "\xd8" = "OE",
  "\xe8|\xe9|\xeb" = "e", # accents on e
  "\xe7" = "c", # c cedille
  "\x83|\x84|\x8a|\x8e" = "?8",
  "\x92" = "?9",
  "\xa6" = "?A",
  "\xb0" = "?B",
  "\xe0|\xe1|\xe5|\xea|\xed" = "?E",
  "\xf1|\xf3|\xf8|\xfd" = "?F"
)
