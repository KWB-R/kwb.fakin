# M A I N ----------------------------------------------------------------------
if (FALSE)
{
  content_dir <- Sys.getenv("FAKIN_CONTENTS")

  # Define "subjects" for which to expect file and folder lists
  subjects <- c("processing", "rawdata", "projects")

  # Initialise result structure
  summaries <- list(files = list(), folders = list())

  for (scope in c("files", "folders")) {

    message("Scope: ", scope)

    # Get paths to files containing file or folder lists, depending on scope
    files <- do.call(c, lapply(paste0(scope, "_", subjects), function(folder) {
      kwb.file::dir_full(kwb.utils::safePath(content_dir, "..", folder))
    }))

    # Fill list of file summaries or folder summaries, depending on scope
    for (file in files) {

      # file <- files_folders[1]
      element <- kwb.utils::removeExtension(basename(file))

      summary_function <- if (scope == "files") {
        get_file_summary
      } else {
        get_folder_summary
      }

      summaries[[scope]][[element]] <- kwb.utils::catAndRun(
        messageText = paste("Creating summary from", file),
        expr = summary_function(read_paths_from_file(file))
      )
    }
  }

  #kwb.fakin:::store(summaries, "summarise_file_info.R")
  #summaries <- kwb.fakin:::restore("summaries", 2)

  # Write summeries to CSV files in the "summaries" folder
  summary_dir <- kwb.utils::safePath(content_dir, "../summaries")

  for (scope_summaries in summaries) {
    for (element in names(scope_summaries)) {
      file <- file.path(summary_dir, sprintf("summary_%s.csv", element))
      write.csv2(scope_summaries[[element]], file = file, row.names = FALSE)
    }
  }
}

# read_paths_from_file ---------------------------------------------------------
read_paths_from_file <- function(file)
{
  full_paths <- readLines(file)
  full_paths <- gsub("^\\\\\\\\medusa", "SERVER", full_paths)
  full_paths <- kwb.utils::rStylePath(full_paths)
  full_paths
}

# split_into_dir_and_basename --------------------------------------------------
split_into_dir_and_basename <- function(full_paths)
{
  parts <- kwb.file:::split_paths(full_paths, dbg = FALSE)

  dir_name <- sapply(parts, function(parts) {
    paste(parts[- length(parts)], collapse = "/")
  })

  base_name <- sapply(parts, kwb.utils::lastElement)

  #stopifnot(identical(dir_name, dirname(full_paths)))
  #stopifnot(identical(base_name, basename(full_paths)))

  kwb.utils::noFactorDataFrame(dirname = dir_name, basename = base_name)
}

# get_file_summary -------------------------------------------------------------
get_file_summary <- function(full_paths, n_levels = 3)
{
  dirname_basename <- split_into_dir_and_basename(full_paths)

  file_info <- kwb.utils::noFactorDataFrame(
    folder = dirname_basename$dirname,
    extension = kwb.utils::fileExtension(dirname_basename$basename)
  )

  subdirs <- kwb.fakin:::toSubdirMatrix(file_info$folder, dbg = FALSE)

  subdirs <- subdirs[, seq_len(n_levels)]

  file_info$folder <- kwb.utils::pasteColumns(as.data.frame(subdirs), sep = "/")

  as.data.frame(dplyr::count(file_info, folder, extension))
}

# get_folder_summary -----------------------------------------------------------
get_folder_summary <- function(full_paths, n_levels = 3)
{
  subdirs <- kwb.fakin:::toSubdirMatrix(full_paths, dbg = FALSE)

  subdirs <- kwb.utils::asNoFactorDataFrame(subdirs[, seq_len(n_levels)])

  file_info <- kwb.utils::noFactorDataFrame(
    folder = kwb.utils::pasteColumns(subdirs, sep = "/")
  )

  as.data.frame(dplyr::count(file_info, folder))
}
