# writePathsToFiles ------------------------------------------------------------
writePathsToFiles <- function(xall, file)
{
  for (i in seq_along(xall)) {

    file <- sub(".txt", paste0("_", LETTERS[i], ".txt"), file)

    kwb.utils::writeText(xall[[i]], file = file)
  }
}
