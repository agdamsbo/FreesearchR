% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{remove_na_attr}
\alias{remove_na_attr}
\title{Remove NA labels}
\usage{
remove_na_attr(data, attr = "label")
}
\arguments{
\item{data}{data}
}
\value{
data.frame
}
\description{
Remove NA labels
}
\examples{
ds <- mtcars |> lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label"))
ds |>
  remove_na_attr() |>
  str()
}
