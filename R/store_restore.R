DATE_TIME_FORMAT <- "_%Y-%m-%d_%H%M%S"
DATE_TIME_PATTERN <- "_\\d{4}-\\d{2}-\\d{2}_\\d{6}"

# clear_storage ----------------------------------------------------------------
clear_storage <- function(name)
{
  files <- get_storage_files(name, extension = "[.][^.]+$")

  n_files <- length(files)

  if (n_files) {

    invisible(kwb.utils::catAndRun(expr = unlink(files), sprintf(
      "Deleting %d files storing objects with name '%s'",
      n_files, name
    )))

  } else {

    cat(sprintf("No objects with name '%s' stored.\n", name))
  }
}

# get_storage_files ------------------------------------------------------------
get_storage_files <- function(name, extension = "[.]RData$")
{
  pattern <- paste0("", name, DATE_TIME_PATTERN, extension)

  dir(get_store_folder(), pattern, full.names = TRUE)
}

# store ------------------------------------------------------------------------
store <- function(x, script_name, dbg = TRUE)
{
  folder <- get_store_folder()

  old_dir <- setwd(folder)

  on.exit(setwd(old_dir))

  name <- deparse(substitute(x))

  kwb.utils::catAndRun("Calculating sha1", dbg = dbg, {

    sha1 <- digest::sha1(x)
  })

  metadata_list <- find_metadata(name, sha1)

  if (length(metadata_list)) {

    existing_file <- kwb.utils::selectElements(metadata_list[[1]], "file")

    message("The object is already stored in ", existing_file)

  } else {

    date_time_extension <- format(Sys.time(), DATE_TIME_FORMAT)

    file_rdata <- paste0(name, date_time_extension, ".RData")

    file_yaml <- paste0(name, date_time_extension, ".yml")

    write_metadata(sha1, folder, file_yaml, script_name)

    write_object(x, name, file_rdata)
  }

  invisible()
}

# get_store_folder -------------------------------------------------------------
get_store_folder <- function(dbg = FALSE)
{
  path <- file.path(dirname(tempdir()), "kwb.fakin.store")

  kwb.utils::createDirectory(path, dbg = dbg)
}

# find_metadata ----------------------------------------------------------------
find_metadata <- function(name, sha1 = NULL)
{
  # Read metadata from all files related to the object
  pattern <- paste0(DATE_TIME_PATTERN, ".yml$")

  metadata_files <- dir(pattern = paste0("^", name, pattern))

  result <- lapply(metadata_files, yaml::read_yaml)

  if (length(result) && ! is.null(sha1)) {

    sha_values <- sapply(result, kwb.utils::selectElements, "sha1")

    result <- result[sha_values == sha1]
  }

  result
}

# write_object -----------------------------------------------------------------
write_object <- function(x, name, file, dbg = TRUE)
{
  kwb.utils::catAndRun(dbg = dbg, paste("Writing", name, "to", file), {

    save(x, file = file)
  })
}

# write_metadata ---------------------------------------------------------------
write_metadata <- function(sha1, folder, file, script_name, dbg = TRUE, ...)
{
  metadata <- list(
    folder = folder,
    file = paste0(kwb.utils::removeExtension(file), ".RData"),
    created = format(Sys.time()),
    creator = script_name,
    sha1 = sha1,
    ...
  )

  kwb.utils::catAndRun(paste("Writing metadata to" , file), {

    yaml::write_yaml(metadata, file)
  })
}

# restore ----------------------------------------------------------------------
restore <- function(name, index = NULL)
{
  files <- get_storage_files(name)

  n_files <- length(files)

  if (n_files == 0) {

    stop("No stored data available.")
  }

  if (n_files > 1 && is.null(index)) {

    metadata_list <- get_metadata(files)

    cat("Set the index argument to decide for a file:\n")

    message_lines <- sprintf(
      "%d: %s (creator: %s)",
      seq_along(files),
      basename(files),
      sapply(metadata_list, kwb.utils::selectElements, "creator")
    )

    kwb.utils::catLines(message_lines)

    return(invisible())
  }

  index <- kwb.utils::defaultIfNULL(index, 1)

  if (! kwb.utils::inRange(index, 1, n_files)) {

    stop("index must be a value between 1 and ", n_files)
  }

  kwb.utils::loadObject(files[index], "x")
}

# get_metadata -----------------------------------------------------------------
get_metadata <- function(file)
{
  stopifnot(is.character(file))

  if (length(file) > 1) {

    return(lapply(file, get_metadata))
  }

  stopifnot(file.exists(file))

  file_yaml <- paste0(kwb.utils::removeExtension(file), ".yml")

  if (file.exists(file_yaml)) {

    yaml::read_yaml(file_yaml)

  } else {

    stop_(
      "No metadata available for file ", file, ".\n",
      "No such file: ", file_yaml
    )
  }
}
