#' Wrapper function to get function from character vector referring to function from namespace. Passed to 'do.call()'
#'
#' @description
#' This function follows the idea from this comment: https://stackoverflow.com/questions/38983179/do-call-a-function-in-r-without-loading-the-package
#' @param x function or function name
#'
#' @return function or character vector
#' @export
#'
#' @examples
#' getfun("stats::lm")
getfun <- function(x) {
  if ("character" %in% class(x)) {
    if (length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      requireNamespace(parts[1])
      getExportedValue(parts[1], parts[2])
    }
  } else {
    x
  }
}

#' Wrapper to save data in RDS, load into specified qmd and render
#'
#' @param data list to pass to qmd
#' @param ... Passed to `quarto::quarto_render()`
#'
#' @return output file name
#' @export
#'
write_quarto <- function(data, ...) {
  # Exports data to temporary location
  #
  # I assume this is more secure than putting it in the www folder and deleting
  # on session end

  # temp <- base::tempfile(fileext = ".rds")
  # readr::write_rds(data, file = here)

  readr::write_rds(data, file = "www/web_data.rds")

  ## Specifying a output path will make the rendering fail
  ## Ref: https://github.com/quarto-dev/quarto-cli/discussions/4041
  ## Outputs to the same as the .qmd file
  quarto::quarto_render(
    execute_params = list(data.file = "web_data.rds"),
    # execute_params = list(data.file = temp),
    ...
  )
}

write_rmd <- function(data, ...) {
  # Exports data to temporary location
  #
  # I assume this is more secure than putting it in the www folder and deleting
  # on session end

  # temp <- base::tempfile(fileext = ".rds")
  # readr::write_rds(data, file = here)

  readr::write_rds(data, file = "www/web_data.rds")

  ## Specifying a output path will make the rendering fail
  ## Ref: https://github.com/quarto-dev/quarto-cli/discussions/4041
  ## Outputs to the same as the .qmd file
  rmarkdown::render(
    params = list(data.file = "web_data.rds"),
    # execute_params = list(data.file = temp),
    ...
  )
}

#' Flexible file import based on extension
#'
#' @param file file name
#' @param consider.na character vector of strings to consider as NAs
#'
#' @return tibble
#' @export
#'
#' @examples
#' read_input("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/data/sample.csv")
read_input <- function(file, consider.na = c("NA", '""', "")) {
  ext <- tools::file_ext(file)

  if (ext == "csv") {
    df <- readr::read_csv(file = file, na = consider.na)
  } else if (ext %in% c("xls", "xlsx")) {
    df <- openxlsx2::read_xlsx(file = file, na.strings = consider.na)
  } else if (ext == "dta") {
    df <- haven::read_dta(file = file)
  } else if (ext == "ods") {
    df <- readODS::read_ods(path = file)
  } else if (ext == "rds") {
    df <- readr::read_rds(file = file)
  } else {
    stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx', '.dta', '.ods' or '.rds'")
  }

  df
}

#' Convert string of arguments to list of arguments
#'
#' @description
#' Idea from the answer: https://stackoverflow.com/a/62979238
#'
#' @param string string to convert to list to use with do.call
#'
#' @return list
#' @export
#'
argsstring2list <- function(string) {
  eval(parse(text = paste0("list(", string, ")")))
}


#' Factorize variables in data.frame
#'
#' @param data data.frame
#' @param vars variables to force factorize
#'
#' @return data.frame
#' @export
factorize <- function(data, vars) {
  if (!is.null(vars)) {
    data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(vars),
          REDCapCAST::as_factor
        )
      )
  } else {
    data
  }
}

dummy_Imports <- function() {
  list(
    MASS::as.fractions(),
    broom::augment(),
    broom.helpers::all_categorical(),
    here::here(),
    cardx::all_of(),
    parameters::ci(),
    DT::addRow(),
    bslib::accordion()
  )
  # https://github.com/hadley/r-pkgs/issues/828
}


#' Title
#'
#' @param data data
#' @param output.format output
#' @param filename filename
#' @param ... passed on
#'
#' @returns data
#' @export
#'
file_export <- function(data, output.format = c("df", "teal", "list"), filename, ...) {
  output.format <- match.arg(output.format)

  filename <- gsub("-", "_", filename)

  if (output.format == "teal") {
    out <- within(
      teal_data(),
      {
        assign(name, value |>
          dplyr::bind_cols(.name_repair = "unique_quiet") |>
          default_parsing())
      },
      value = data,
      name = filename
    )

    datanames(out) <- filename
  } else if (output.format == "df") {
    out <- data |>
      default_parsing()
  } else if (output.format == "list") {
    out <- list(
      data = data,
      name = filename
    )

    out <- c(out, ...)
  }

  out
}


#' Default data parsing
#'
#' @param data data
#'
#' @returns data.frame or tibble
#' @export
#'
#' @examples
#' mtcars |> str()
#' mtcars |>
#'   default_parsing() |>
#'   str()
default_parsing <- function(data) {
  name_labels <- lapply(data, \(.x) REDCapCAST::get_attr(.x, attr = "label"))

  out <- data |>
    REDCapCAST::parse_data() |>
    REDCapCAST::as_factor() |>
    REDCapCAST::numchar2fct(numeric.threshold = 8, character.throshold = 10) |>
    REDCapCAST::as_logical() |>
    REDCapCAST::fct_drop()

  purrr::map2(out, name_labels, \(.x, .l){
    if (!(is.na(.l) | .l == "")) {
      REDCapCAST::set_attr(.x, .l, attr = "label")
    } else {
      attr(x = .x, which = "label") <- NULL
      .x
    }
    # REDCapCAST::set_attr(data = .x, label = .l,attr = "label", overwrite = FALSE)
  }) |> dplyr::bind_cols()
}

#' Remove NA labels
#'
#' @param data data
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' ds <- mtcars |> lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label"))
#' ds |>
#'   remove_na_attr() |>
#'   str()
remove_na_attr <- function(data, attr = "label") {
  out <- data |> lapply(\(.x){
    ls <- REDCapCAST::get_attr(data = .x, attr = attr)
    if (is.na(ls) | ls == "") {
      attr(x = .x, which = attr) <- NULL
    }
    .x
  })

  dplyr::bind_cols(out)
}

#' Removes columns with completenes below cutoff
#'
#' @param data data frame
#' @param cutoff numeric
#'
#' @returns data frame
#' @export
#'
#' @examples
#' data.frame(a = 1:10, b = NA, c = c(2, NA)) |> remove_empty_cols(cutoff = .5)
remove_empty_cols <- function(data, cutoff = .7) {
  filter <- apply(X = data, MARGIN = 2, FUN = \(.x){
    sum(as.numeric(!is.na(.x))) / length(.x)
  }) >= cutoff
  data[filter]
}


#' Append list with named index
#'
#' @param data data to add to list
#' @param list list
#' @param index index name
#'
#' @returns list
#' @export
#'
#' @examples
#' ls_d <- list(test = c(1:20))
#' ls_d <- list()
#' data.frame(letters[1:20], 1:20) |> append_list(ls_d, "letters")
#' letters[1:20] |> append_list(ls_d, "letters")
append_list <- function(data, list, index) {
  ## This will overwrite and not warn
  ## Not very safe, but convenient to append code to list
  if (index %in% names(list)) {
    list[[index]] <- data
    out <- list
  } else {
    out <- setNames(c(list, list(data)), c(names(list), index))
  }
  out
}


#' Get missingsness fraction
#'
#' @param data data
#'
#' @returns numeric vector
#' @export
#'
#' @examples
#' c(NA, 1:10, rep(NA, 3)) |> missing_fraction()
missing_fraction <- function(data) {
  NROW(data[is.na(data)]) / NROW(data)
}



#' Ultra short data dascription
#'
#' @param data
#'
#' @returns character vector
#' @export
#'
#' @examples
#' data.frame(
#'   sample(1:8, 20, TRUE),
#'   sample(c(1:8, NA), 20, TRUE)
#' ) |> data_description()
data_description <- function(data) {
  data <- if (shiny::is.reactive(data)) data() else data

  n <- nrow(data)
  n_var <- ncol(data)
  n_complete <- sum(complete.cases(data))
  p_complete <- n_complete/n

  sprintf(
    i18n("Data has %s observations and %s variables, with %s (%s%%) complete cases."),
    n,
    n_var,
    n_complete,
    signif(100 * p_complete, 3)
  )
}

#' Drop-in replacement for the base::sort_by with option to remove NAs
#'
#' @param x x
#' @param y y
#' @param na.rm remove NAs
#' @param ... passed to base_sort_by
#'
#' @returns vector
#' @export
#'
#' @examples
#' sort_by(c("Multivariable", "Univariable"),c("Univariable","Minimal","Multivariable"))
sort_by <- function(x,y,na.rm=FALSE,...){
  out <- base::sort_by(x,y,...)
  if (na.rm==TRUE){
    out[!is.na(out)]
  } else {
    out
  }
}


get_ggplot_label <- function(data,label){
  assertthat::assert_that(ggplot2::is.ggplot(data))
  data$labels[[label]]
}


#' Return if available
#'
#' @param data vector
#' @param default assigned value for missings
#'
#' @returns vector
#' @export
#'
#' @examples
#' NULL |> if_not_missing("new")
#' c(2,"a",NA) |> if_not_missing()
#' "See" |> if_not_missing()
if_not_missing <- function(data,default=NULL){
  if (length(data)>1){
    Reduce(c,lapply(data,if_not_missing))
  } else if (is.na(data) || is.null(data)){
    return(default)
  } else {
    return(data)
  }
}
