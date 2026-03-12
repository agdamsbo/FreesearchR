#' Easily launch the FreesearchR app
#'
#' @description
#' All data.frames in the global environment will be accessible through the app.
#'
#' @param include_globalenv flag to include global env (local data) as option
#' when loading data
#' @param data_limit_default default data set observations limit
#' @param data_limit_upper data set observations upper limit
#' @param data_limit_lower data set observations lower limit
#' @param ... passed on to `shiny::runApp()`
#'
#' @returns shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' launch_FreesearchR(launch.browser = TRUE)
#' }
launch_FreesearchR <- function(include_globalenv = TRUE,
                               data_limit_default = 1000,
                               data_limit_upper = 100000,
                               data_limit_lower = 1,
                               ...) {
  Sys.setenv(
    INCLUDE_GLOBALENV = include_globalenv,
    DATA_LIMIT_DEFAULT = data_limit_default,
    DATA_LIMIT_UPPER = data_limit_upper,
    DATA_LIMIT_LOWER = data_limit_lower
  )

  appDir <- system.file("apps", "FreesearchR", package = "FreesearchR")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing `FreesearchR`.",
         call. = FALSE)
  }

  a <- shiny::runApp(appDir = paste0(appDir, "/app.R"), ...)
  return(invisible(a))
}


## Helper to set env variables
get_config <- function(var_name, default = NULL) {
  val <- Sys.getenv(var_name, unset = NA_character_)

  # Only use env var if it is explicitly set and non-empty
  if (!is.na(val) && nzchar(trimws(val))) {
    if (is.logical(default)) return(to_logical(val))
    if (is.numeric(default)) return(as.numeric(val))
    return(val)
  }

  if (!is.null(default)) {
    return(default)
  }

  stop(paste("Required config variable not set:", var_name))
}

to_logical <- function(x) {
  result <- switch(tolower(trimws(as.character(x))),
                   "true"  = , "1" = , "yes" = TRUE,
                   "false" = , "0" = , "no"  = FALSE,
                   NA
  )
  if (is.na(result)) stop(paste("Cannot coerce to logical:", x))
  result
}


## File loader - based on the module, uses hard coded default values
load_file <- function(path) {
  read_fns <- list(
    ods  = "import_ods",
    dta  = "import_dta",
    csv  = "import_delim",
    tsv  = "import_delim",
    txt  = "import_delim",
    xls  = "import_xls",
    xlsx = "import_xls",
    rds  = "import_rds"
  )

  ext <- tolower(tools::file_ext(path))

  if (!ext %in% names(read_fns)) {
    message("Unsupported file type, skipping: ", basename(path), " (.", ext, ")")
    return(NULL)
  }

  read_fn <- read_fns[[ext]]

  parameters <- list(
    file     = path,
    sheet    = 1,
    skip     = 0,
    dec      = ".",
    encoding = "unknown"
  )

  # Trim parameters to only those accepted by the target function
  parameters <- parameters[which(names(parameters) %in% rlang::fn_fmls_names(get(read_fn)))]

  result <- tryCatch(
    rlang::exec(read_fn, !!!parameters),
    error = function(e) {
      # Fall back to rio::import
      message("Primary loader failed for ", basename(path), ", trying rio::import")
      tryCatch(
        rio::import(path),
        error = function(e2) {
          message("Failed to load ", basename(path), ": ", e2$message)
          NULL
        }
      )
    }
  )

  if (!is.null(result) && NROW(result) < 1) {
    message("File loaded but contains no rows, skipping: ", basename(path))
    return(NULL)
  }

  result
}


load_folder <- function(folder = "/app/data", envir = .GlobalEnv) {
  if (is.null(folder) || !dir.exists(folder)) {
    message("No data folder found, skipping load")
    return(invisible(NULL))
  }

  files <- list.files(folder, full.names = TRUE)
  if (length(files) == 0) {
    message("Data folder is empty, skipping load")
    return(invisible(NULL))
  }

  loaded <- vapply(files, function(file) {
    result <- load_file(file)
    if (is.null(result))
      return(FALSE)
    name <- tools::file_path_sans_ext(basename(file))
    assign(name, default_parsing(result), envir = envir)
    TRUE
  }, logical(1))

  message(sprintf(
    "Loaded %d/%d files from %s",
    sum(loaded),
    length(files),
    folder
  ))
  invisible(loaded)
}
