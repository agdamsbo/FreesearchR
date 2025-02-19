#' Create a regression model programatically
#'
#' @param data data set
#' @param fun Name of function as character vector or function to use for model creation.
#' @param vars character vector of variables to include
#' @param outcome.str Name of outcome variable. Character vector.
#' @param auto.mode Make assumptions on function dependent on outcome data format. Overwrites other arguments.
#' @param formula.str Formula as string. Passed through 'glue::glue'. If given, 'outcome.str' and 'vars' are ignored. Optional.
#' @param args.list List of arguments passed to 'fun' with 'do.call'.
#' @param ... ignored for now
#'
#' @importFrom stats as.formula
#'
#' @return object of standard class for fun
#' @export
#' @rdname regression_model
#'
#' @examples
#' gtsummary::trial |>
#'   regression_model(outcome.str = "age")
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "age",
#'     auto.mode = FALSE,
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = NULL
#'   )
#' gtsummary::trial |>
#'   default_parsing() |>
#'   regression_model(
#'     outcome.str = "trt",
#'     auto.mode = FALSE,
#'     fun = "stats::glm",
#'     args.list = list(family = binomial(link = "logit"))
#'   )
#' m <- mtcars |>
#'   default_parsing() |>
#'   regression_model(
#'     outcome.str = "mpg",
#'     auto.mode = FALSE,
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
#'     args.list = NULL,
#'     vars = c("mpg", "cyl")
#'   )
#'   broom::tidy(m)
regression_model <- function(data,
                             outcome.str,
                             auto.mode = FALSE,
                             formula.str = NULL,
                             args.list = NULL,
                             fun = NULL,
                             vars = NULL,
                             ...) {
  if (!is.null(formula.str)) {
    if (formula.str == "") {
      formula.str <- NULL
    }
  }

  ## This will handle if outcome is not in data for nicer shiny behavior
  if (!outcome.str %in% names(data)){
    outcome.str <- names(data)[1]
    print("outcome is not in data, first column is used")
  }

  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% outcome.str]
  } else {
    if (outcome.str %in% vars) {
      vars <- vars[!vars %in% outcome.str]
    }
    data <- data |> dplyr::select(dplyr::all_of(c(vars, outcome.str)))
  }

  if (!is.null(formula.str)) {
    formula.glue <- glue::glue(formula.str)
  } else {
    assertthat::assert_that(outcome.str %in% names(data),
      msg = "Outcome variable is not present in the provided dataset"
    )
    formula.glue <- glue::glue("{outcome.str}~{paste(vars,collapse='+')}")
  }

  # Formatting character variables as factor
  # Improvement should add a missing vector to format as NA
  data <- data |>
    purrr::map(\(.x){
      if (is.character(.x)) {
        suppressWarnings(REDCapCAST::as_factor(.x))
      } else {
        .x
      }
    }) |>
    dplyr::bind_cols(.name_repair = "unique_quiet")

  if (is.null(fun)) auto.mode <- TRUE

  if (auto.mode) {
    if (is.numeric(data[[outcome.str]])) {
      fun <- "stats::lm"
    } else if (is.factor(data[[outcome.str]])) {
      if (length(levels(data[[outcome.str]])) == 2) {
        fun <- "stats::glm"
        args.list <- list(family = stats::binomial(link = "logit"))
      } else if (length(levels(data[[outcome.str]])) > 2) {
        fun <- "MASS::polr"
        args.list <- list(
          Hess = TRUE,
          method = "logistic"
        )
      } else {
        stop("The provided output variable only has one level")
      }
    } else {
      stop("Output variable should be either numeric or factor for auto.mode")
    }
  }

  assertthat::assert_that("character" %in% class(fun),
    msg = "Please provide the function as a character vector."
  )

  # browser()
  out <- do.call(
    getfun(fun),
    c(
      list(
        data = data,
        formula = as.formula(formula.glue)
      ),
      args.list
    )
  )

  # Recreating the call
  # out$call <-  match.call(definition=eval(parse(text=fun)), call(fun, data = 'data',formula = as.formula(formula.str),args.list))

  return(out)
}

#' Create a regression model programatically
#'
#' @param data data set
#' @param fun Name of function as character vector or function to use for model creation.
#' @param vars character vector of variables to include
#' @param outcome.str Name of outcome variable. Character vector.
#' @param args.list List of arguments passed to 'fun' with 'do.call'.
#' @param ... ignored for now
#'
#' @importFrom stats as.formula
#' @rdname regression_model
#'
#' @return object of standard class for fun
#' @export
#'
#' @examples
#' \dontrun{
#' gtsummary::trial |>
#'   regression_model_uv(outcome.str = "age")
#' gtsummary::trial |>
#'   regression_model_uv(
#'     outcome.str = "age",
#'     fun = "stats::lm",
#'     args.list = NULL
#'   )
#' m <- gtsummary::trial |> regression_model_uv(
#'   outcome.str = "trt",
#'   fun = "stats::glm",
#'   args.list = list(family = stats::binomial(link = "logit"))
#' )
#' lapply(m,broom::tidy) |> dplyr::bind_rows()
#' }
regression_model_uv <- function(data,
                                outcome.str,
                                args.list = NULL,
                                fun = NULL,
                                vars = NULL,
                                ...) {

  ## This will handle if outcome is not in data for nicer shiny behavior
  if (!outcome.str %in% names(data)){
    outcome.str <- names(data)[1]
    print("outcome is not in data, first column is used")
  }

  if (!is.null(vars)) {
    data <- data |>
      dplyr::select(dplyr::all_of(
        unique(c(outcome.str, vars))
      ))
  }

  if (is.null(args.list)) {
    args.list <- list()
  }

  if (is.null(fun)) {
    if (is.numeric(data[[outcome.str]])) {
      fun <- "stats::lm"
    } else if (is.factor(data[[outcome.str]])) {
      if (length(levels(data[[outcome.str]])) == 2) {
        fun <- "stats::glm"
        args.list <- list(family = stats::binomial(link = "logit"))
      } else if (length(levels(data[[outcome.str]])) > 2) {
        fun <- "MASS::polr"
        args.list <- list(
          Hess = TRUE,
          method = "logistic"
        )
      } else {
        stop("The provided output variable only has one level")
      }
    } else {
      stop("Output variable should be either numeric or factor for auto.mode")
    }
  }

  assertthat::assert_that("character" %in% class(fun),
    msg = "Please provide the function as a character vector."
  )

  out <- names(data)[!names(data) %in% outcome.str] |>
    purrr::map(\(.var){
      do.call(
        regression_model,
        c(
          list(
            data = data[match(c(outcome.str, .var), names(data))],
            outcome.str = outcome.str
          ),
          args.list
        )
      )
    })

  return(out)
}


### HELPERS

#' Outcome data type assessment
#'
#' @param data data
#'
#' @returns outcome type
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   lapply(outcome_type)
outcome_type <- function(data) {
  cl_d <- class(data)
  if (any(c("numeric", "integer") %in% cl_d)) {
    out <- "continuous"
  } else if (identical("factor", cl_d)) {
    if (length(levels(data)) == 2) {
      out <- "dichotomous"
    } else if (length(levels(data)) > 2) {
      out <- "ordinal"
    }
  } else {
    out <- "unknown"
  }

  out
}


#' Implemented functions
#'
#' @description
#' Library of supported functions. The list name and "descr" element should be
#' unique for each element on list.
#'
#'
#' @returns list
#' @export
#'
#' @examples
#' supported_functions()
supported_functions <- function() {
  list(
    lm = list(
      descr = "Linear regression model",
      design = "cross-sectional",
      out.type = "continuous",
      fun = "stats::lm",
      args.list = NULL,
      formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
      table.fun = "gtsummary::tbl_regression",
      table.args.list = list(exponentiate = FALSE)
    ),
    glm = list(
      descr = "Logistic regression model",
      design = "cross-sectional",
      out.type = "dichotomous",
      fun = "stats::glm",
      args.list = list(family = stats::binomial(link = "logit")),
      formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
      table.fun = "gtsummary::tbl_regression",
      table.args.list = list()
    ),
    polr = list(
      descr = "Ordinal logistic regression model",
      design = "cross-sectional",
      out.type = "ordinal",
      fun = "MASS::polr",
      args.list = list(
        Hess = TRUE,
        method = "logistic"
      ),
      formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
      table.fun = "gtsummary::tbl_regression",
      table.args.list = list()
    )
  )
}


#' Get possible regression models
#'
#' @param data data
#'
#' @returns character vector
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::pull("cyl") |>
#'   possible_functions(design = "cross-sectional")
#'
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::select("cyl") |>
#'   possible_functions(design = "cross-sectional")
possible_functions <- function(data, design = c("cross-sectional")) {
  # browser()
  if (is.data.frame(data)) {
    data <- data[[1]]
  }

  design <- match.arg(design)
  type <- outcome_type(data)

  design_ls <- supported_functions() |>
    lapply(\(.x){
      if (design %in% .x$design) {
        .x
      }
    })

  if (type == "unknown") {
    out <- type
  } else {
    out <- design_ls |>
      lapply(\(.x){
        if (type %in% .x$out.type) {
          .x$descr
        }
      }) |>
      unlist()
  }
  unname(out)
}


#' Get the function options based on the selected function description
#'
#' @param data vector
#'
#' @returns list
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::pull(mpg) |>
#'   possible_functions(design = "cross-sectional") |>
#'   (\(.x){
#'     .x[[1]]
#'   })() |>
#'   get_fun_options()
get_fun_options <- function(data) {
  descrs <- supported_functions() |>
    lapply(\(.x){
      .x$descr
    }) |>
    unlist()
  supported_functions() |>
    (\(.x){
      .x[match(data, descrs)]
    })()
}


#' Wrapper to create regression model based on supported models
#'
#' @description
#' Output is a concatenated list of model information and model
#'
#'
#' @param data data
#' @param outcome.str name of outcome variable
#' @param fun.descr Description of chosen function matching description in
#' "supported_functions()"
#' @param fun name of custom function. Default is NULL.
#' @param formula.str custom formula glue string. Default is NULL.
#' @param args.list custom character string to be converted using
#' argsstring2list() or list of arguments. Default is NULL.
#' @param ... ignored
#'
#' @returns list
#' @export
#' @rdname regression_model
#'
#' @examples
#' \dontrun{
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "age",
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = NULL
#'   )
#' ls <- regression_model_list(data = default_parsing(mtcars), outcome.str = "cyl", fun.descr = "Ordinal logistic regression model")
#' summary(ls$model)
#'
#' ls <- regression_model_list(data = default_parsing(gtsummary::trial), outcome.str = "trt", fun.descr = "Logistic regression model")
#' tbl <- gtsummary::tbl_regression(ls$model, exponentiate = TRUE)
#' m <- gtsummary::trial |>
#'   default_parsing() |>
#'   regression_model(
#'     outcome.str = "trt",
#'     fun = "stats::glm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = list(family = stats::binomial(link = "logit"))
#'   )
#' tbl2 <- gtsummary::tbl_regression(m, exponentiate = TRUE)
#' broom::tidy(ls$model)
#' broom::tidy(m)
#' }
regression_model_list <- function(data,
                                  outcome.str,
                                  fun.descr,
                                  fun = NULL,
                                  formula.str = NULL,
                                  args.list = NULL,
                                  vars = NULL,
                                  ...) {
  options <- get_fun_options(fun.descr) |>
    (\(.x){
      .x[[1]]
    })()

  ## Custom, specific fun, args and formula options

  if (is.null(formula.str)) {
    formula.str.c <- options$formula.str
  } else {
    formula.str.c <- formula.str
  }

  if (is.null(fun)) {
    fun.c <- options$fun
  } else {
    fun.c <- fun
  }

  if (is.null(args.list)) {
    args.list.c <- options$args.list
  } else {
    args.list.c <- args.list
  }

  if (is.character(args.list.c)) args.list.c <- argsstring2list(args.list.c)

  ## Handling vars to print code

  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% outcome.str]
  } else {
    if (outcome.str %in% vars) {
      vars <- vars[!vars %in% outcome.str]
    }
  }

  model <- do.call(
    regression_model,
    list(
      data = data,
      outcome.str = outcome.str,
      fun = fun.c,
      formula.str = formula.str.c,
      args.list = args.list.c
    )
  )

  code <- glue::glue(
    "{fun.c}({paste(Filter(length,list(glue::glue(formula.str.c),'data = data',list2str(args.list.c))),collapse=', ')})"
  )

  list(
    options = options,
    model = model,
    code = code
  )
}

list2str <- function(data) {
  out <- purrr::imap(data, \(.x, .i){
    if (is.logical(.x)) {
      arg <- .x
    } else {
      arg <- glue::glue("'{.x}'")
    }
    glue::glue("{.i} = {arg}")
  }) |>
    unlist() |>
    paste(collapse = (", "))

  if (out == "") {
    return(NULL)
  } else {
    out
  }
}


#' @returns list
#' @export
#' @rdname regression_model
#'
#' @examples
#' \dontrun{
#' gtsummary::trial |> regression_model_uv(
#'   outcome.str = "trt",
#'   fun = "stats::glm",
#'   args.list = list(family = stats::binomial(link = "logit"))
#' ) |> lapply(broom::tidy) |> dplyr::bind_rows()
#' ms <- regression_model_uv_list(data = default_parsing(mtcars), outcome.str = "mpg", fun.descr = "Linear regression model")
#' lapply(ms$model,broom::tidy) |> dplyr::bind_rows()
#' }
regression_model_uv_list <- function(data,
                                     outcome.str,
                                     fun.descr,
                                     fun = NULL,
                                     formula.str = NULL,
                                     args.list = NULL,
                                     vars = NULL,
                                     ...) {
  options <- get_fun_options(fun.descr) |>
    (\(.x){
      .x[[1]]
    })()

  ## Custom, specific fun, args and formula options

  if (is.null(formula.str)) {
    formula.str.c <- options$formula.str
  } else {
    formula.str.c <- formula.str
  }

  if (is.null(fun)) {
    fun.c <- options$fun
  } else {
    fun.c <- fun
  }

  if (is.null(args.list)) {
    args.list.c <- options$args.list
  } else {
    args.list.c <- args.list
  }

  if (is.character(args.list.c)) args.list.c <- argsstring2list(args.list.c)

  ## Handling vars to print code

  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% outcome.str]
  } else {
    if (outcome.str %in% vars) {
      vars <- vars[!vars %in% outcome.str]
    }
  }

  # assertthat::assert_that("character" %in% class(fun),
  #   msg = "Please provide the function as a character vector."
  # )

  # model <- do.call(
  #   regression_model,
  #   c(
  #     list(data = data),
  #     list(outcome.str = outcome.str),
  #     list(fun = fun.c),
  #     list(formula.str = formula.str.c),
  #     args.list.c
  #   )
  # )

  model <- vars |>
    lapply(\(.var){
      do.call(
        regression_model,
        list(
          data = data[c(outcome.str, .var)],
          outcome.str = outcome.str,
          fun = fun.c,
          formula.str = formula.str.c,
          args.list = args.list.c
        )
      )
    })


  vars <- "."

  code_raw <- glue::glue(
    "{fun.c}({paste(Filter(length,list(glue::glue(formula.str.c),'data = .d',list2str(args.list.c))),collapse=', ')})"
  )

  code <- glue::glue("lapply(data,function(.d){code_raw})")

  list(
    options = options,
    model = model,
    code = code
  )
}
