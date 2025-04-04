% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcap_read_shiny_module.R
\name{drop_empty_event}
\alias{drop_empty_event}
\title{Drop empty events from REDCap export}
\usage{
drop_empty_event(data, event = "redcap_event_name")
}
\arguments{
\item{data}{data}

\item{event}{"redcap_event_name", "redcap_repeat_instrument" or
"redcap_repeat_instance"}
}
\value{
data.frame
}
\description{
Drop empty events from REDCap export
}
