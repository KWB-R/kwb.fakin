# merge_read_and_write_matrices ------------------------------------------------

#' Merge Read and Write Permission Matrices
#'
#' @param matrix_read matrix of 0 with 1 at positions with read-permissions
#' @param matrix_write matrix of 0 with 2 at positions with write-permissions
#'
#' @return matrix of character with "" at positions without permissions, "-" at
#'   positions with read-permissions, "|" at positions with write-permissions
#'   and "+" at positions with read- and write-permissions
#'
#' @export
#'
#' @examples
#'
#' # Get example matrices
#' matrices <- get_example_read_and_write_matrices()
#'
#' # Overlay example matrices
#' merge_read_and_write_matrices(matrices$read, matrices$write)
#'
merge_read_and_write_matrices <- function(matrix_read, matrix_write)
{
  # Resize matrices so that they have all rows and all columns
  permissions <- lapply(
    X = list(read = matrix_read, write = matrix_write),
    FUN = kwb.utils::assertRowsAndColumns,
    row_names = sort(c(rownames(matrix_read), rownames(matrix_write))),
    col_names = sort(c(colnames(matrix_read), colnames(matrix_write))),
    fill_value = 0
  )

  # We can now simply add!
  permissions_num <- permissions$read + permissions$write

  # Make a copy to be filled with character
  permissions_chr <- permissions_num

  # Set symbols according to value (1 = read, 2 = write, 3 = read and write)
  permissions_chr[permissions_num == 0] <- ""  # no permissions
  permissions_chr[permissions_num == 1] <- "-" # read
  permissions_chr[permissions_num == 2] <- "|" # write
  permissions_chr[permissions_num == 3] <- "+" # read and write

  # Show the result matrix
  permissions_chr
}

# get_example_read_and_write_matrices ------------------------------------------

#' Get Example Matrices of Read- and Write-Permissions
#'
#' @export
#'
get_example_read_and_write_matrices <- function()
{
  # Initialise example matrices for read and write with 0
  read <- kwb.utils::createMatrix(paste0("path_", 1:5), paste0("user_", 5:8))
  write <- kwb.utils::createMatrix(paste0("path_", 4:7), paste0("user_", 1:6))

  # Set matrices at random positions to 1
  read[sample(n <- length(read), n/2)] <- 1
  write[sample(n <- length(write), n/2)] <- 2

  list(read = read, write = write)
}
