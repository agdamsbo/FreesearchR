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
launch_FreesearchR <- function(inlcude_globalenv = TRUE,
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
  # First check environment variables (set by Docker)
  val <- Sys.getenv(var_name, unset = NA)

  if (!is.na(val) && nzchar(val)) {
    return(val)
  }

  # Fall back to default (can be overridden when launching from R)
  if (!is.null(default)) {
    return(default)
  }

  stop(paste("Required config variable not set:", var_name))
}
