# replaceSpecial ---------------------------------------------------------------
replaceSpecial <- function(x, all = TRUE, dbg = TRUE) {

  catIf <- kwb.utils::catIf

  if (all) {

    catIf(dbg, "Replacing special characters in", length(x), "strings ... ")
    kwb.utils::multiSubstitute(x, replacements(), useBytes = TRUE)
    catIf(dbg, "ok.\n")

  } else {

    catIf(dbg, "Replacing 2 Byte chars in", length(x), "strings ... ")
    x <- kwb.utils::multiSubstitute(x, replacements2(), useBytes = TRUE)
    catIf(dbg, "ok.\n")

    catIf(dbg, "Replacing 1 Byte chars in", length(x), "strings ... ")
    x <- kwb.utils::multiSubstitute(x, replacements1())
    catIf(dbg, "ok.\n")
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
