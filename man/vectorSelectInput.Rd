% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_SelectInput.R
\name{vectorSelectInput}
\alias{vectorSelectInput}
\title{A selectizeInput customized for named vectors}
\usage{
vectorSelectInput(
  inputId,
  label,
  choices,
  selected = "",
  ...,
  placeholder = "",
  onInitialize
)
}
\arguments{
\item{inputId}{passed to \code{\link[shiny]{selectizeInput}}}

\item{label}{passed to \code{\link[shiny]{selectizeInput}}}

\item{choices}{A named \code{vector} from which fields should be populated}

\item{selected}{default selection}

\item{...}{passed to \code{\link[shiny]{selectizeInput}}}

\item{placeholder}{passed to \code{\link[shiny]{selectizeInput}} options}

\item{onInitialize}{passed to \code{\link[shiny]{selectizeInput}} options}
}
\value{
a \code{\link[shiny]{selectizeInput}} dropdown element
}
\description{
A selectizeInput customized for named vectors
}
\examples{
if (shiny::interactive()) {
shinyApp(
  ui = fluidPage(
    shiny::uiOutput("select"),
    tableOutput("data")
  ),
  server = function(input, output) {
    output$select <- shiny::renderUI({
      vectorSelectInput(
        inputId = "variable", label = "Variable:",
        data = c(
          "Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear"
        )
      )
    })

    output$data <- renderTable(
      {
        mtcars[, c("mpg", input$variable), drop = FALSE]
      },
      rownames = TRUE
    )
  }
)
}
}
