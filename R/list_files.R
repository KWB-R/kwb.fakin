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

    paths <- kwb.utils::catAndRun(paste("Scanning all files in", root), dir(
      root, all.files = TRUE, full.names = TRUE, recursive = TRUE, no.. = TRUE
    ))

    kwb.utils::writeText(paths, file, "paths to")
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

  kwb.utils::writeText(batch_lines, batch_file, "batch file")

  batch_file
}
