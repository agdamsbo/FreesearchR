% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_read_shiny_module.R
\name{is_valid_token}
\alias{is_valid_token}
\title{Validate REDCap token}
\usage{
is_valid_token(token, pattern_env = NULL, nchar = 32)
}
\arguments{
\item{token}{token}

\item{pattern_env}{pattern}
}
\value{
logical
}
\description{
Validate REDCap token
}
\examples{
token <- paste(sample(c(1:9, LETTERS[1:6]), 32, TRUE), collapse = "")
is_valid_token(token)
}
