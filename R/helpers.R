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

write_rmd <- function(data, ..., params.args=NULL) {
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
    params = modifyList(list(data.file = "web_data.rds",version=app_version()),params.args),
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
    df <- readxl::read_excel(file = file, na.strings = consider.na)
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
#' factorize(mtcars, names(mtcars))
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
    bslib::accordion(),
    NHANES::NHANES(),
    stRoke::add_padding()
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
#' ds <- mtcars |>
#'   lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label")) |>
#'   dplyr::bind_cols()
#' ds |>
#'   remove_empty_attr() |>
#'   str()
#' mtcars |>
#'   lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label")) |>
#'   remove_empty_attr() |>
#'   str()
#'
remove_empty_attr <- function(data) {
  if (is.data.frame(data)) {
    data |>
      lapply(remove_empty_attr) |>
      dplyr::bind_cols()
  } else if (is.list(data)) {
    data |> lapply(remove_empty_attr)
  } else {
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
  p_complete <- signif(100 * n_complete / n, 3)

  glue::glue(i18n$t("{data_text} has {n} observations and {n_var} variables, with {n_complete} ({p_complete} %) complete cases."))
  # sprintf(
  #   "%s has %s observations and %s variables, with %s (%s%%) complete cases.",
  #   data_text,
  #   n,
  #   n_var,
  #   n_complete,
  #   p_complete
  # )
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
  assertthat::assert_that(all(type %in% names(data_types())))

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
  assertthat::assert_that(ggplot2::is_ggplot(data))
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



#' Test if element is identical to the previous
#'
#' @param data data. vector, data.frame or list
#' @param no.name logical to remove names attribute before testing
#'
#' @returns logical vector
#' @export
#'
#' @examples
#' c(1, 1, 2, 3, 3, 2, 4, 4) |> is_identical_to_previous()
#' mtcars[c(1, 1, 2, 3, 3, 2, 4, 4)] |> is_identical_to_previous()
#' list(1, 1, list(2), "A", "a", "a") |> is_identical_to_previous()
is_identical_to_previous <- function(data, no.name = TRUE) {
  if (is.data.frame(data)) {
    lagged <- data.frame(FALSE, data[seq_len(length(data) - 1)])
  } else {
    lagged <- c(FALSE, data[seq_len(length(data) - 1)])
  }

  vapply(seq_len(length(data)), \(.x){
    if (isTRUE(no.name)) {
      identical(unname(lagged[.x]), unname(data[.x]))
    } else {
      identical(lagged[.x], data[.x])
    }
  }, FUN.VALUE = logical(1))
}


#' Simplified version of the snakecase packages to_snake_case
#'
#' @param data character string vector
#'
#' @returns vector
#' @export
#'
#' @examples
#' c("foo bar", "fooBar21", "!!Foo'B'a-r", "foo_bar", "F  OO bar") |> simple_snake()
simple_snake <- function(data){
  gsub("[\\s+]","_",gsub("[^\\w\\s:-]", "", tolower(data), perl=TRUE), perl=TRUE)
}

#' Data type assessment.
#'
#' @description
#' These are more overall than the native typeof. This is used to assess a more
#' meaningful "clinical" data type.
#'
#' @param data vector or data.frame. if data frame, each column is evaluated.
#'
#' @returns outcome type
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   lapply(data_type)
#' mtcars |>
#'   default_parsing() |>
#'   data_type()
#' c(1, 2) |> data_type()
#' 1 |> data_type()
#' c(rep(NA, 10)) |> data_type()
#' sample(1:100, 50) |> data_type()
#' factor(letters[1:20]) |> data_type()
#' as.Date(1:20) |> data_type()
data_type <- function(data) {
  if (is.data.frame(data)) {
    sapply(data, data_type)
  } else {
    cl_d <- class(data)
    l_unique <- length(unique(na.omit(data)))
    if (all(is.na(data))) {
      out <- "empty"
    } else if (l_unique < 2) {
      out <- "monotone"
    } else if (any(c("factor", "logical") %in% cl_d) | l_unique == 2) {
      if (identical("logical", cl_d) | l_unique == 2) {
        out <- "dichotomous"
      } else {
        # if (is.ordered(data)) {
        #   out <- "ordinal"
        # } else {
        out <- "categorical"
        # }
      }
    } else if (identical(cl_d, "character")) {
      out <- "text"
    } else if (any(c("hms", "Date", "POSIXct", "POSIXt") %in% cl_d)) {
      out <- "datetime"
    } else if (l_unique > 2) {
      ## Previously had all thinkable classes
      ## Now just assumes the class has not been defined above
      ## any(c("numeric", "integer", "hms", "Date", "timediff") %in% cl_d) &
      out <- "continuous"
    } else {
      out <- "unknown"
    }

    out
  }
}

#' Recognised data types from data_type
#'
#' @returns vector
#' @export
#'
#' @examples
#' data_types()
data_types <- function() {
  list(
    "empty" = list(descr="Variable of all NAs",classes="Any class"),
    "monotone" = list(descr="Variable with only one unique value",classes="Any class"),
    "dichotomous" = list(descr="Variable with only two unique values",classes="Any class"),
    "categorical"= list(descr="Factor variable",classes="factor (ordered or unordered)"),
    "text"= list(descr="Character variable",classes="character"),
    "datetime"= list(descr="Variable of time, date or datetime values",classes="hms, Date, POSIXct and POSIXt"),
    "continuous"= list(descr="Numeric variable",classes="numeric, integer or double"),
    "unknown"= list(descr="Anything not falling within the previous",classes="Any other class")
  )
}
