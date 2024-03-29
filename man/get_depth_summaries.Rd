% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_depth_summaries.R
\name{get_depth_summaries}
\alias{get_depth_summaries}
\title{Get File Number and Size Summary per Folder Depth}
\usage{
get_depth_summaries(file_data, project_dir, max_depth = NULL)
}
\arguments{
\item{file_data}{data frame as returned by \code{\link{read_file_info}}}

\item{project_dir}{path by which to filter the paths in \code{file_data},
passed to \code{fakin.path.app:::prepare_for_treemap}}

\item{max_depth}{maximum depth for which to calculate a summary. If
\code{NULL} (default), all summaries are created for all available path
depths}
}
\value{
list of data frames
}
\description{
Get File Number and Size Summary per Folder Depth
}
