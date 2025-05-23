% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{write_quarto}
\alias{write_quarto}
\title{Wrapper to save data in RDS, load into specified qmd and render}
\usage{
write_quarto(data, ...)
}
\arguments{
\item{data}{list to pass to qmd}

\item{...}{Passed to \code{quarto::quarto_render()}}
}
\value{
output file name
}
\description{
Wrapper to save data in RDS, load into specified qmd and render
}
