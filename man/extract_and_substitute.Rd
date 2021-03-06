% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_properties.R
\name{extract_and_substitute}
\alias{extract_and_substitute}
\title{Extract Substring Matching Pattern and Replace}
\usage{
extract_and_substitute(pattern, replacement, x)
}
\arguments{
\item{pattern}{pattern to be matched against the values in \code{x}. The
pattern may contain pairs of parentheses surrounding parts of the pattern
which can be referred to in the replacement string with \\1, \\2, etc., see
example.}

\item{replacement}{string of the form \code{prop1:value1+prop2:value2+...}
defining property names and values. In \code{value1}, \code{value2}, etc.
it may be referred to parts of the matching string that correspond to
the parts of the pattern enclosed in parentheses, see example.}

\item{x}{vector of character in which to look for substrings to extract}
}
\description{
Extract Substring Matching Pattern and Replace
}
\examples{
kwb.fakin:::extract_and_substitute(
  pattern = "key_(ab(c|d))",
  replacement = "alphabet:true+full:\\\\1+char2:\\\\2",
  x = c("noch viel mehr key_abc", "key_abd", "abe")
)
}
\keyword{internal}
