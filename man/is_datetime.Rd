% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut-variable-dates.R
\name{is_datetime}
\alias{is_datetime}
\title{Test is date/datetime/time}
\usage{
is_datetime(data)
}
\arguments{
\item{data}{data}
}
\value{
factor
}
\description{
Test is date/datetime/time
}
\examples{
vapply(REDCapCAST::redcapcast_data, is_datetime, logical(1))
}
