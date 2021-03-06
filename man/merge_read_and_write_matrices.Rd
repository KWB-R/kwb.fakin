% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_permissions.R
\name{merge_read_and_write_matrices}
\alias{merge_read_and_write_matrices}
\title{Merge Read and Write Permission Matrices}
\usage{
merge_read_and_write_matrices(matrix_read, matrix_write)
}
\arguments{
\item{matrix_read}{matrix of 0 with 1 at positions with read-permissions}

\item{matrix_write}{matrix of 0 with 2 at positions with write-permissions}
}
\value{
matrix of character with "" at positions without permissions, "-" at
  positions with read-permissions, "|" at positions with write-permissions
  and "+" at positions with read- and write-permissions
}
\description{
Merge Read and Write Permission Matrices
}
\examples{

# Get example matrices
matrices <- get_example_read_and_write_matrices()

# Overlay example matrices
merge_read_and_write_matrices(matrices$read, matrices$write)

}
