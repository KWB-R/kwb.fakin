# read_file_paths --------------------------------------------------------------
read_file_paths <- function(file, metadata = NULL)
{
  if (is.null(metadata)) {
    metadata <- guess_file_metadata(file)
  }

  if (! is_valid_path_file(metadata)) {
    return (NULL)
  }

  metadata$encoding <- kwb.utils::defaultIfNULL(
    metadata$encoding, guess_expected_encoding(
      file, expected = c("ISO-8859-1", "WINDOWS-1252")
    )
  )

  if (metadata$ncol == 1) {

    paths <- read_paths(file, fileEncoding = metadata$encoding)
    result <- kwb.utils::noFactorDataFrame(path = paths)
    result$type <- guess_file_path_type(x = result$path)
    result$size <- ifelse(result$type == "directory", 0L, 2^20)

  } else {

    result <- read_file_info(
      file, sep = metadata$sep, fileEncoding = metadata$encoding_fread
    )
  }

  result
}

# is_valid_path_file -----------------------------------------------------------
is_valid_path_file <- function(metadata)
{
  get <- kwb.utils::selectElements

  is_valid <- get(metadata, "paths") && ! get(metadata, "forbidden")

  if (! is_valid) {

    format_string <- paste0(
      "The file does not seem to contain valid paths. Returning NULL.\n",
      "File: %s\n",
      "Folder: %s\n",
      "First %d rows:\n%s\n"
    )

    file <- kwb.utils::getAttribute(metadata, "file")
    first_rows <- kwb.utils::getAttribute(metadata, "first_rows")

    message(sprintf(
      format_string, basename(file), dirname(file), length(first_rows),
      paste(collapse = "\n", first_rows)
    ))
  }

  is_valid
}

# guess_file_path_type ---------------------------------------------------------
guess_file_path_type <- function(x)
{
  kwb.utils::catAndRun("Guessing file path type", {
    has_final_slash <- grepl_bytes("/$", x)
    has_extension <- kwb.utils::fileExtension(x) != ""
    ifelse(has_final_slash | ! has_extension, "directory", "file")
  })
}

