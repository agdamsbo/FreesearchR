# dependencies
library(apexcharter)
library(toastui)

spark_data <- mtcars |>
  (\(.x){
    dplyr::tibble(
      name = names(.x),
      vals = as.list(.x)
    )
  })()

ui <- fluidPage(
  toastui::datagridOutput("tbl")
)

server <- function(input, output) {
  output$tbl <- toastui::renderDatagrid(
    spark_data |>
      toastui::datagrid() |>
      toastui::grid_sparkline(
        column = "vals",
        renderer = function(data) {
          apex(data.frame(x = 1, y = data), aes(x, y), type = "box") |>
            ax_chart(sparkline = list(enabled = TRUE)) |>
            ax_plotOptions(
              bar = bar_opts(horizontal=TRUE)
            )
        }
      )
  )
}

shinyApp(ui = ui, server = server)
