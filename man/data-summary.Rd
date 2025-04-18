% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-summary.R
\name{data-summary}
\alias{data-summary}
\alias{data_summary_ui}
\alias{data_summary_server}
\title{Data summary module}
\usage{
data_summary_ui(id)

data_summary_server(id, data, color.main, color.sec, ...)
}
\arguments{
\item{id}{Module id. (Use 'ns("id")')}

\item{data}{data}

\item{color.main}{main color}

\item{color.sec}{secondary color}

\item{...}{arguments passed to create_overview_datagrid}
}
\value{
Shiny ui module

shiny server module
}
\description{
Data summary module
}
