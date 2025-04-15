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
#' @examples
#' argsstring2list("A=1:5,b=2:4")
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
#'
#' @examples
#' factorize(mtcars,names(mtcars))
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
#' head(starwars, 5) |> str()
#' starwars |>
#'   default_parsing() |>
#'   head(5) |>
#'   str()
default_parsing <- function(data) {
  name_labels <- lapply(data, \(.x) REDCapCAST::get_attr(.x, attr = "label"))
  # browser()
  out <- data |>
    setNames(make.names(names(data), unique = TRUE)) |>
    ## Temporary step to avoid nested list and crashing
    remove_nested_list() |>
    REDCapCAST::parse_data() |>
    REDCapCAST::as_factor() |>
    REDCapCAST::numchar2fct(numeric.threshold = 8, character.throshold = 10) |>
    REDCapCAST::as_logical() |>
    REDCapCAST::fct_drop()

  set_column_label(out, setNames(name_labels, names(out)), overwrite = FALSE)

  # purrr::map2(
  #   out,
  #   name_labels[names(name_labels) %in% names(out)],
  #   \(.x, .l){
  #     if (!(is.na(.l) | .l == "")) {
  #       REDCapCAST::set_attr(.x, .l, attr = "label")
  #     } else {
  #       attr(x = .x, which = "label") <- NULL
  #       .x
  #     }
  #     # REDCapCAST::set_attr(data = .x, label = .l,attr = "label", overwrite = FALSE)
  #   }
  # ) |> dplyr::bind_cols()
}

#' Remove empty/NA attributes
#'
#' @param data data
#'
#' @returns data of same class as input
#' @export
#'
#' @examples
#' ds <- mtcars |> lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label")) |> dplyr::bind_cols()
#' ds |>
#'   remove_empty_attr() |>
#'   str()
#'  mtcars |> lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label")) |> remove_empty_attr() |>
#'   str()
#'
remove_empty_attr <- function(data) {
  if (is.data.frame(data)){
    data |> lapply(remove_empty_attr) |> dplyr::bind_cols()
  } else if (is.list(data)){
    data |> lapply(remove_empty_attr)
  }else{
  attributes(data)[is.na(attributes(data))] <- NULL
  data
  }
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
data_description <- function(data, data_text = "Data") {
  data <- if (shiny::is.reactive(data)) data() else data

  n <- nrow(data)
  n_var <- ncol(data)
  n_complete <- sum(complete.cases(data))
  p_complete <- n_complete / n

  sprintf(
    "%s has %s observations and %s variables, with %s (%s%%) complete cases.",
    data_text,
    n,
    n_var,
    n_complete,
    signif(100 * p_complete, 3)
  )
}


#' Filter function to filter data set by variable type
#'
#' @param data data frame
#' @param type vector of data types (recognised: data_types)
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' default_parsing(mtcars) |>
#'   data_type_filter(type = c("categorical", "continuous")) |>
#'   attributes()
#' default_parsing(mtcars) |>
#'   data_type_filter(type = NULL) |>
#'   attributes()
#' \dontrun{
#' default_parsing(mtcars) |> data_type_filter(type = c("test", "categorical", "continuous"))
#' }
data_type_filter <- function(data, type) {
  ## Please ensure to only provide recognised data types
  assertthat::assert_that(all(type %in% data_types()))

  if (!is.null(type)) {
    out <- data[data_type(data) %in% type]
    code <- rlang::call2("data_type_filter", !!!list(type = type), .ns = "FreesearchR")
    attr(out, "code") <- code
  } else {
    out <- data
  }
  out
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
#' sort_by(c("Multivariable", "Univariable"), c("Univariable", "Minimal", "Multivariable"))
sort_by <- function(x, y, na.rm = FALSE, ...) {
  out <- base::sort_by(x, y, ...)
  if (na.rm == TRUE) {
    out[!is.na(out)]
  } else {
    out
  }
}


get_ggplot_label <- function(data, label) {
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
#' c(2, "a", NA) |> if_not_missing()
#' "See" |> if_not_missing()
if_not_missing <- function(data, default = NULL) {
  if (length(data) > 1) {
    Reduce(c, lapply(data, if_not_missing))
  } else if (is.na(data) || is.null(data)) {
    return(default)
  } else {
    return(data)
  }
}


#' Merge list of expressions
#'
#' @param data list
#'
#' @returns expression
#' @export
#'
#' @examples
#' list(
#'   rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
#'   rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
#' ) |> merge_expression()
merge_expression <- function(data) {
  Reduce(
    f = function(x, y) rlang::expr(!!x %>% !!y),
    x = data
  )
}

#' Reduce character vector with the native pipe operator or character string
#'
#' @param data list
#'
#' @returns character string
#' @export
#'
#' @examples
#' list(
#'   "mtcars",
#'   rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
#'   rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
#' ) |>
#'   lapply(expression_string) |>
#'   pipe_string() |>
#'   expression_string("data<-")
pipe_string <- function(data, collapse = "|>\n") {
  if (is.list(data)) {
    Reduce(
      f = function(x, y) glue::glue("{x}{collapse}{y}"),
      x = data
    )
  } else {
    data
  }
}

#' Deparses expression as string, substitutes native pipe and adds assign
#'
#' @param data expression
#'
#' @returns string
#' @export
#'
#' @examples
#' list(
#'   as.symbol(paste0("mtcars$", "mpg")),
#'   rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
#'   rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
#' ) |>
#'   merge_expression() |>
#'   expression_string()
expression_string <- function(data, assign.str = "") {
  exp.str <- if (is.call(data)) deparse(data) else data
  # browser()
  out <- paste0(assign.str, gsub("%>%", "|>\n", paste(gsub('"', "'", paste(exp.str, collapse = "")), collapse = "")))
  gsub(" |`", "", out)
}


#' Very simple function to remove nested lists, like when uploading .rds
#'
#' @param data data
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' dplyr::tibble(a = 1:10, b = rep(list("a"), 10)) |> remove_nested_list()
#' dplyr::tibble(a = 1:10, b = rep(list(c("a", "b")), 10)) |> as.data.frame()
remove_nested_list <- function(data) {
  data[!sapply(data, is.list)]
}




#' (Re)label columns in data.frame
#'
#' @param data data.frame to be labelled
#' @param label named list or vector
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' ls <- list("mpg" = "", "cyl" = "Cylinders", "disp" = "", "hp" = "", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")
#' ls2 <- c("mpg" = "", "cyl" = "Cylinders", "disp" = "", "hp" = "Horses", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")
#' ls3 <- c("mpg" = "", "cyl" = "", "disp" = "", "hp" = "Horses", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")
#' mtcars |>
#'   set_column_label(ls) |>
#'   set_column_label(ls2) |>
#'   set_column_label(ls3)
#' rlang::expr(FreesearchR::set_column_label(label = !!ls3)) |> expression_string()
set_column_label <- function(data, label, overwrite = TRUE) {
  purrr::imap(data, function(.data, .name) {
    ls <- if (is.list(label)) unlist(label) else label
    ls[ls == ""] <- NA
    if (.name %in% names(ls)) {
      out <- REDCapCAST::set_attr(.data, unname(ls[.name]), attr = "label", overwrite = overwrite)
      remove_empty_attr(out)
    } else {
      .data
    }
  }) |> dplyr::bind_cols(.name_repair = "unique_quiet")
}


#' Append a column to a data.frame
#'
#' @param data data
#' @param column new column (vector) or data.frame with 1 column
#' @param name new name (pre-fix)
#' @param index desired location. May be "left", "right" or numeric index.
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' mtcars |>
#'   dplyr::mutate(mpg_cut = mpg) |>
#'   append_column(mtcars$mpg, "mpg_cutter")
append_column <- function(data, column, name, index = "right") {
  assertthat::assert_that(NCOL(column) == 1)
  assertthat::assert_that(length(index) == 1)

  if (index == "right") {
    index <- ncol(data) + 1
  } else if (index == "left") {
    index <- 1
  } else if (is.numeric(index)) {
    if (index > ncol(data)) {
      index <- ncol(data) + 1
    }
  } else {
    index <- ncol(data) + 1
  }

  ## Identifying potential naming conflicts
  nm_conflicts <- names(data)[startsWith(names(data), name)]
  ## Simple attemt to create new unique name
  if (length(nm_conflicts) > 0) {
    name <- glue::glue("{name}_{length(nm_conflicts)+1}")
  }
  ## If the above not achieves a unique name, the generic approach is used
  if (name %in% names(data)) {
    name <- make.names(c(name, names(data)), unique = TRUE)[1]
  }
  new_df <- setNames(data.frame(column), name)

  list(
    data[seq_len(index - 1)],
    new_df,
    if (!index > ncol(data)) data[index:ncol(data)]
  ) |>
    dplyr::bind_cols()
}
