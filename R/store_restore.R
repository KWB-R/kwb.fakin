# store ------------------------------------------------------------------------
store <- function(x, script_name, dbg = TRUE)
{
  folder <- get_store_folder()

  old_dir <- setwd(folder)
  on.exit(setwd(old_dir))

  name <- deparse(substitute(x))

  data_file <- function(xx, sha1) paste0(xx, "_", sha1, ".RData")
  text_file <- function(xx, sha1) paste0(xx, "_", sha1, ".txt")

  kwb.utils::catIf(dbg, "Calculating sha1... ")

  sha1 <- digest::sha1(x)

  kwb.utils::catIf(dbg, "ok.\n")

  if (file.exists(data_file(name, sha1))) {

    message("The object is already stored in ", data_file(name, sha1))

  } else {

    write_object(x, name, data_file(name, sha1))

    write_metadata(x, folder, text_file(name, sha1), script_name)
  }
}

# get_store_folder -------------------------------------------------------------
get_store_folder <- function()
{
  kwb.utils::safePath(kwb.utils::desktop(), "tmp")
}

# write_object -----------------------------------------------------------------
write_object <- function(x, name, file, dbg = TRUE)
{
  kwb.utils::catIf(dbg, "Writing", name, "to", file, "... ")

  save(x, file = file)

  kwb.utils::catIf(dbg, "ok.\n")
}

# write_metadata ---------------------------------------------------------------
write_metadata <- function(sha1, folder, file, script_name, dbg = TRUE)
{
  kwb.utils::writeText(
    x = sprintf(
      "Folder: %s\nFile: %s\nCreated: %s\nCreator: %s\nsha1: %s\n",
      folder, file, Sys.time(), script_name, sha1
    ),
    file = file.path(folder, file),
    type = "metadata to",
    dbg = dbg
  )
}

# read_sha1 --------------------------------------------------------------------
read_sha1 <- function(file)
{
  if (! file.exists(file)) {

    stop("No metadata available. Missing file: ", file)
  }

  content <- readLines(file)

  index <- grep("^sha1:", content)

  if (length(index) == 0) {

    stop("No information on sha1 available in ", file)
  }

  gsub("^sha1:\\s+", "", content[index])
}

# restore ----------------------------------------------------------------------
restore <- function(name, index = NULL)
{
  folder <- get_store_folder()

  files <- dir(folder, paste0("", name, "_[0-9a-f]{40}\\.RData$"))

  n_files <- length(files)

  if (n_files == 0) {

    stop("No stored data available.")
  }

  if (n_files > 1 && is.null(index)) {

    cat("Set the index argument to decide for a file:\n")
    cat(paste(seq_along(files), files, sep = ": ", collapse = "\n"))

    return(invisible())
  }

  index <- kwb.utils::defaultIfNULL(index, 1)

  if (! kwb.utils::inRange(index, 1, n_files)) {

    stop("index must be a value between 1 and ", n_files)
  }

  kwb.utils::loadObject(kwb.utils::safePath(folder, files[index]), "x")
}
