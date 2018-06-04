# list_files -------------------------------------------------------------------

#' Get File Paths Recursively (Windows only!)
#'
#' Get a full list of files below a \code{root} directory using the \code{dir}
#' function and write it to a given \code{file}
#'
#' @param root path to the directory from which to start "downwards" and
#'   recursively for files and folders.
#' @param file path to the result file (text) to which the paths are to be
#'   written.
#' @param use_batch if \code{TRUE} (default), a batch file is written (by
#'   default to \code{list_files.bat} in \code{tempdir()}) and run to perform
#'   the dir command
#'
#' @export
#'
list_files <- function(root, file, use_batch = TRUE)
{
  # Check the output directory
  kwb.utils::safePath(dirname(file))

  cat_time <- function(tag) {
    cat(paste0("\n", tag, ":"), as.character(Sys.time()), "\n\n")
  }

  cat_time("Start")

  if (use_batch) {

    # Write a batch file
    batchfile <- write_batch_list_files(root, file)

    # Run the batchfile
    system2(batchfile)

  } else {

    # Set locale to "C" as it is done in the batch file to achieve the same
    # sorting of paths

    locale_all <- strsplit(Sys.getlocale("LC_ALL"), ";")[[1]]
    Sys.setlocale("LC_ALL", "C")
    on.exit(for (locale_one in locale_all) {
      parts <- strsplit(locale_one, "=")[[1]]
      Sys.setlocale(parts[1], parts[2])
    })

    cat("Scanning all files in", root, "... ")
    paths <- dir(
      root, all.files = TRUE, full.names = TRUE, recursive = TRUE, no.. = TRUE
    )
    cat("ok.\n")

    cat("Writing paths to", file, "... ")
    writeLines(paths, file)
    cat("ok.\n")
  }

  cat_time("End")
}

# write_batch_list_files -------------------------------------------------------
write_batch_list_files <- function(
  root, output_file, batch_file = file.path(tempdir(), "list_files.bat")
)
{
  error_file <- gsub("\\.txt$", ".err", output_file)

  batch_lines <- c(
    '@echo OFF',
    '@echo %time%',
    'echo Getting the paths recursively...',
    sprintf('SET ROOT="%s"', kwb.utils::windowsPath(root)),
    'REM Change to code page 1250 (ANSI for Central Europe)',
    'chcp 1250',
    'REM /S -> including subdirectories',
    'REM /B -> simple output format',
    'REM /A:-D -> do not put directory paths in output',
    'SET LC_ALL=C',
    paste(
      sprintf('dir %%ROOT%% /S /B /A:-D sortorder:N'),
      sprintf('2> "%s"', kwb.utils::windowsPath(error_file)),
      sprintf('| sed -e "s/\\\\\\/\\\\//g"'),
      sprintf('| sort'),
      sprintf('> "%s"', kwb.utils::windowsPath(output_file))
    ),
    '@echo %time%'
  )

  cat("Writing batch file", batch_file, "... ")
  writeLines(batch_lines, batch_file)
  cat("ok.\n")

  batch_file
}

# read_paths -------------------------------------------------------------------

#' Read Paths From File
#'
#' Read paths from file, convert backslashes to slashes and sort the paths
#' (by default)
#'
#' @param file full path to the text file containing paths
#' @param encoding passed to \code{\link[base]{readLines}}
#' @param do_sort if \code{TRUE} (default), the vector of paths is sorted
#'   alphanumerically.
#'
#' @return vector of character
#'
#' @export
#'
read_paths <- function(file, encoding = NULL, do_sort = TRUE)
{
  if (is.null(encoding)) {

    encodings <- utils::localeToCharset()

    if (length(encodings) > 1) {
      cat("Suggested encodings:", kwb.utils::stringList(encodings), "\n")
    }

    encoding <- encodings[1]
  }

  cat("Selected encoding:", kwb.utils::hsQuoteChr(encoding), "\n")

  cat("Reading paths from\n ", file, "...\n")
  paths <- readLines(file, encoding = encodings[2])
  cat("ok.", length(paths), "lines have been read.\n")

  cat("Checking for (back-)slashes... ")
  has_slash <- grepl("/", paths)
  has_backslash <- grepl("[\\]", paths)
  all_slash <- all(has_slash)
  all_backslash <- all(has_backslash)
  cat("ok.\n")

  if (! all_slash) {

    if (all_backslash) {

      cat("Converting backslashes to slashes... ")
      paths <- gsub("[\\]", "/", paths)
      cat("ok.\n")

    } else {

      print(table(has_slash))
      print(table(has_backslash))

      stop("There are paths with backslash but not all paths have a backslash!")
    }
  }

  if (do_sort) {

    cat("Sorting paths... ")
    paths <- sort(paths)
    cat("ok.\n")
  }

  paths
}

# printFreqs -------------------------------------------------------------------
printFreqs <- function(x, maxchar = 80)
{
  x$path <- substr(x$path, 1, maxchar)
  print(x)
}

# writePathsToFiles ------------------------------------------------------------
writePathsToFiles <- function(xall, file)
{
  for (i in seq_along(xall)) {

    file.out <- sub(".txt", paste0("_", LETTERS[i], ".txt"), file)

    cat("Writing", file.out, "...")
    writeLines(xall[[i]], file.out)
    cat("ok.\n")
  }
}

# logResultIf ------------------------------------------------------------------
logResultIf <- function(dbg, x, y)
{
  if (dbg) {

    kwb.utils::catLines(c("\n### x:", x))
    kwb.utils::catLines(c("\n### y:", y))
    cat("\n### str(dict):\n")
    utils::str(kwb.utils::getAttribute(y, "dict"))
  }
}
