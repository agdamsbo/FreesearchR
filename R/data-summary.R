#' Data summary module
#'
#' @param id Module id. (Use 'ns("id")')
#'
#' @name data-summary
#' @returns Shiny ui module
#' @export
data_summary_ui <- function(id) {
  ns <- NS(id)

  toastui::datagridOutput(outputId = ns("tbl_summary"))
}


#'
#' @param data data
#' @param color.main main color
#' @param color.sec secondary color
#' @param ... arguments passed to create_overview_datagrid
#'
#' @name data-summary
#' @returns shiny server module
#' @export
data_summary_server <- function(id,
                                data,
                                color.main,
                                color.sec,
                                ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      output$tbl_summary <-
        toastui::renderDatagrid(
          {
            shiny::req(data())
            data() |>
            overview_vars() |>
            create_overview_datagrid(...) |>
            add_sparkline(
              column = "vals",
              color.main = color.main,
              color.sec = color.sec
            )
          }
            )

    }
  )
}

#' Add sparkline to datagrid
#'
#' @param grid grid
#' @param column clumn to transform
#'
#' @returns datagrid
#' @export
#'
#' @examples
#' grid <- mtcars |>
#'   default_parsing() |>
#'   overview_vars() |>
#'   toastui::datagrid() |>
#'   add_sparkline()
#' grid
add_sparkline <- function(grid, column = "vals", color.main = "#2a8484", color.sec = "#84EF84") {
  out <- toastui::grid_sparkline(
    grid = grid,
    column = column,
    renderer = function(data) {
      data_cl <- class(data)
      if (all(sapply(data,is.na))){
        type <- "line"
        ds <- data.frame(x = NA, y = NA)
        horizontal <- FALSE
      } else if (identical(data_cl, "factor")) {
        type <- "column"
        s <- summary(data)
        ds <- data.frame(x = names(s), y = s)
        horizontal <- FALSE
      } else if (identical(data_cl, "logical")) {
        type <- "column"
        s <- table(data)
        ds <- data.frame(x = names(s), y = as.vector(s))
        horizontal <- FALSE
      } else if (any(c("numeric", "integer") %in% data_cl)) {
        if (is_consecutive(data)) {
          type <- "line"
          ds <- data.frame(x = NA, y = NA)
          horizontal <- FALSE
        } else {
          type <- "box"
          ds <- data.frame(x = 1, y = data)
          horizontal <- TRUE
        }
      } else if (any(c("Date", "POSIXct", "POSIXt", "hms", "difftime") %in% data_cl)) {
        type <- "line"
        ds <- data.frame(x = seq_along(data), y = data)
        horizontal <- FALSE
      } else {
        type <- "line"
        ds <- data.frame(x = NA, y = NA)
        horizontal <- FALSE
      }
      apexcharter::apex(
        ds,
        apexcharter::aes(x, y),
        type = type,
        auto_update = TRUE
      ) |>
        apexcharter::ax_chart(sparkline = list(enabled = TRUE)) |>
        apexcharter::ax_plotOptions(
          boxPlot = apexcharter::boxplot_opts(color.upper = color.sec, color.lower = color.main),
          bar = apexcharter::bar_opts(horizontal = horizontal)
        ) |>
        apexcharter::ax_colors(
          c(color.main, color.sec)
        )
    }
  )

  toastui::grid_columns(
    grid = out,
    columns = column,
    minWidth = 200
  )
}

#' Checks if elements in vector are equally spaced as indication of ID
#'
#' @param data vector
#'
#' @returns logical
#' @export
#'
#' @examples
#' 1:10 |> is_consecutive()
#' sample(1:100,40) |> is_consecutive()
is_consecutive <- function(data){
  suppressWarnings(length(unique(diff(as.numeric(data))))==1)
}

#' Create a data overview data.frame ready for sparklines
#'
#' @param data data
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' mtcars |> overview_vars()
overview_vars <- function(data) {
  data <- as.data.frame(data)

  dplyr::tibble(
    icon = data_type(data),
    type = icon,
    name = names(data),
    n_missing = unname(colSums(is.na(data))),
    p_complete = 1 - n_missing / nrow(data),
    n_unique = get_n_unique(data),
    vals = as.list(data)
  )
}

#' Create a data overview datagrid
#'
#' @param data data
#'
#' @returns datagrid
#' @export
#'
#' @examples
#' mtcars |>
#'   overview_vars() |>
#'   create_overview_datagrid()
create_overview_datagrid <- function(data,...) {
  # browser()
  gridTheme <- getOption("datagrid.theme")
  if (length(gridTheme) < 1) {
    datamods:::apply_grid_theme()
  }
  on.exit(toastui::reset_grid_theme())

  col.names <- names(data)

  std_names <- c(
    "Name" = "name",
    "Icon" = "icon",
    "Type" = "type",
    "Missings" = "n_missing",
    "Complete" = "p_complete",
    "Unique" = "n_unique",
    "Distribution" = "vals"
  )

  headers <- lapply(col.names, \(.x){
    if (.x %in% std_names) {
      names(std_names)[match(.x, std_names)]
    } else {
      .x
    }
  }) |> unlist()

  grid <- toastui::datagrid(
    data = data,
    theme = "default",
    colwidths = "fit",
    ...
  )

  grid <- toastui::grid_columns(
    grid = grid,
    columns = col.names,
    header = headers,
    resizable = TRUE
  )

  grid <- toastui::grid_columns(
    grid = grid,
    columns = "vals",
    width = 120
  )

  grid <- toastui::grid_columns(
    grid = grid,
    columns = "icon",
    header = " ",
    align = "center",sortable = FALSE,
    width = 40
  )

  grid <- add_class_icon(
    grid = grid,
    column = "icon",
    fun = type_icons
  )

  grid <- toastui::grid_format(
    grid = grid,
    "p_complete",
    formatter = toastui::JS("function(obj) {return (obj.value*100).toFixed(0) + '%';}")
  )

  ## This could obviously be extended, which will added even more complexity.

  grid <- toastui::grid_filters(
    grid = grid,
    column = "name",
    # columns = unname(std_names[std_names!="vals"]),
    showApplyBtn = FALSE,
    showClearBtn = TRUE,
    type = "text"
  )


  return(grid)
}

#' Convert class grid column to icon
#'
#' @param grid grid
#' @param column column
#'
#' @returns datagrid
#' @export
#'
#' @examples
#' mtcars |>
#'   overview_vars() |>
#'   toastui::datagrid() |>
#'   add_class_icon()
add_class_icon <- function(grid, column = "class", fun=class_icons) {
  out <- toastui::grid_format(
    grid = grid,
    column = column,
    formatter = function(value) {
      lapply(
        X = value,
        FUN = fun
      )
    }
  )

  toastui::grid_columns(
    grid = out,
    header = NULL,
    columns = column,
    width = 60
  )
}


#' Get data class icons
#'
#' @param x character vector of data classes
#'
#' @returns
#' @export
#'
#' @examples
#' "numeric" |> class_icons()
#' default_parsing(mtcars) |> sapply(class) |> class_icons()
class_icons <- function(x) {
  if (length(x)>1){
    sapply(x,class_icons)
  } else {
  if (identical(x, "numeric")) {
    shiny::icon("calculator")
  } else if (identical(x, "factor")) {
    shiny::icon("chart-simple")
  } else if (identical(x, "integer")) {
    shiny::icon("arrow-down-1-9")
  } else if (identical(x, "character")) {
    shiny::icon("arrow-down-a-z")
  } else if (identical(x, "logical")) {
    shiny::icon("toggle-off")
  } else if (any(c("Date", "POSIXct", "POSIXt") %in% x)) {
    shiny::icon("calendar-days")
  } else if ("hms" %in% x) {
    shiny::icon("clock")
  } else {
    shiny::icon("table")
  }}
}

#' Get data type icons
#'
#' @param x character vector of data classes
#'
#' @returns
#' @export
#'
#' @examples
#' "ordinal" |> type_icons()
#' default_parsing(mtcars) |> sapply(data_type) |> type_icons()
type_icons <- function(x) {
  if (length(x)>1){
    sapply(x,class_icons)
  } else {
    if (identical(x, "continuous")) {
      shiny::icon("calculator")
    } else if (identical(x, "categorical")) {
      shiny::icon("chart-simple")
    } else if (identical(x, "ordinal")) {
      shiny::icon("arrow-down-1-9")
    } else if (identical(x, "text")) {
      shiny::icon("arrow-down-a-z")
    } else if (identical(x, "dichotomous")) {
      shiny::icon("toggle-off")
    } else if (identical(x,"datetime")) {
      shiny::icon("calendar-days")
    } else if (identical(x,"id")) {
      shiny::icon("id-card")
    } else {
      shiny::icon("table")
    }
    }
}
