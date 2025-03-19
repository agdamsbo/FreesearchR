#' Easily launch the FreesearchR app
#'
#' @description
#' All data.frames in the global environment will be accessible through the app.
#'
#' @param ... passed on to `shiny::runApp()`
#'
#' @returns shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' shiny_FreesearchR(launch.browser = TRUE)
#' }
launch_FreesearchR <- function(...){
  appDir <- system.file("apps", "FreesearchR", package = "FreesearchR")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing `FreesearchR`.", call. = FALSE)
  }

  a <- shiny::runApp(appDir = paste0(appDir,"/app.R"), ...)
  return(invisible(a))
}
