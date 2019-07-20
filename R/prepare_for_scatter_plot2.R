# prepare_for_scatter_plot2 ----------------------------------------------------
prepare_for_scatter_plot2 <- function(file_data)
{
  stopifnot(inherits(file_data, "pathlist"))

  kwb.utils::checkForMissingColumns(file_data@data, c("type", "size"))

  file_data <- file_data[file_data@data$type == "file"]

  if (length(file_data) == 0) {
    return(NULL)
  }

  files <- pathlist::filename(file_data)

  kwb.utils::noFactorDataFrame(
    root = pathlist::toplevel(file_data),
    folder = pathlist::folder(file_data),
    file = files,
    extension = kwb.utils::fileExtension(files),
    depth = file_data@depths,
    size = file_data@data[, "size", drop = FALSE]
  )
}
