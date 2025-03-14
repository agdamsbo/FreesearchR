% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_freesearcheR.R
\name{shiny_freesearcheR}
\alias{shiny_freesearcheR}
\title{Launch the freesearcheR tool locally}
\usage{
shiny_freesearcheR(...)
}
\arguments{
\item{...}{arguments passed on to \code{shiny::runApp()}}
}
\value{
shiny app
}
\description{
All data.frames in the global environment will be accessible through the app.
}
\examples{
\dontrun{
data(mtcars)
shiny_freesearcheR(launch.browser = TRUE)
}
}
