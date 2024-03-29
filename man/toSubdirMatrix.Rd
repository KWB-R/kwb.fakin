% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{toSubdirMatrix}
\alias{toSubdirMatrix}
\title{Deprecated.}
\usage{
toSubdirMatrix(paths, fill.value = "", result_type = "matrix", dbg = TRUE)
}
\arguments{
\item{paths}{vector of path strings}

\item{fill.value}{value used to fill empty cells of the result matrix}

\item{result_type}{one of \code{c("matrix", "data.frame")}, specifying the
type of object to be returned}

\item{dbg}{if \code{TRUE} debug messages are shown}
}
\value{
matrix or data frame, depending on \code{result_type}
}
\description{
This function is deprecated. Please use kwb.file::to_subdir_matrix() instead.
}
\keyword{internal}
