# restoreEncoding --------------------------------------------------------------
restoreEncoding <- function(x, FUN, ...)
{
  encodings <- Encoding(x)

  result <- FUN(x, ...)

  Encoding(result) <- encodings

  result
}

# utf8_to_iso_8859_1 -----------------------------------------------------------
utf8_to_iso_8859_1 <- function(x)
{
  iconv(x, from = "UTF-8", to = "ISO-8859-1")
}
