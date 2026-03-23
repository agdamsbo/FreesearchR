# version_check.R
#
# Runs a one-time version check at app startup and returns a ready-made
# shinyWidgets::alert() UI element that can be placed directly in the UI
# definition -- no server(), no renderUI(), no uiOutput() required.
#
# Because the check runs outside server(), it executes once when the app
# process starts, so the banner is present immediately on first render with
# no loading delay.
#
# Version detection uses two strategies, tried in order:
#   1. utils::packageVersion() -- works when the package is installed locally.
#   2. app_version argument    -- explicit fallback for environments where the
#      package is not installed (e.g. shinyapps.io). Pass the result of your
#      app_version() function here.
#
# Quick start:
#
#   # global.R (or top of app.R, before ui / server)
#   source("version_check.R")
#   version_banner <- check_app_version(
#     github_user = "your-github-username",
#     github_repo = "your-repo-name",
#     app_version = app_version()   # fallback for shinyapps.io
#   )
#
#   # ui.R -- drop the result anywhere in the UI tree
#   fluidPage(
#     version_banner,
#     ...
#   )
#
#   # Verbose / debug mode -- always show the banner:
#   version_banner <- check_app_version(
#     github_user = "your-github-username",
#     github_repo = "your-repo-name",
#     app_version = app_version(),
#     verbose     = TRUE
#   )


# -- Internal helpers ----------------------------------------------------------

#' Check internet connectivity
#'
#' @return Logical; TRUE if an internet connection is available.
.has_internet <- function() {
  tryCatch({
    con <- url("https://api.github.com", open = "r")
    close(con)
    TRUE
  }, error = function(e) FALSE)
}


#' Fetch the latest release version from a GitHub repository
#'
#' @param github_user GitHub username or organisation.
#' @param github_repo Repository name.
#'
#' @return A character string with the version tag (e.g. "1.2.0"), or NULL on
#'   failure.
.get_latest_github_version <- function(github_user, github_repo) {
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/releases/latest",
    github_user,
    github_repo
  )

  tryCatch({
    response  <- readLines(url(api_url), warn = FALSE)
    json_text <- paste(response, collapse = "")

    tag <- regmatches(
      json_text,
      regexpr('"tag_name"\\s*:\\s*"([^"]+)"', json_text)
    )

    if (length(tag) == 0 || nchar(tag) == 0) return(NULL)

    # Strip a leading "v" if present (e.g. "v1.2.0" -> "1.2.0")
    sub('^"tag_name"\\s*:\\s*"v?([^"]+)"$', "\\1", tag)
  }, error = function(e) NULL)
}


#' Resolve the current app version
#'
#' Tries two strategies in order:
#' \enumerate{
#'   \item \code{utils::packageVersion(package_name)} -- works when the package
#'     is installed locally (development, local \code{runApp()}).
#'   \item \code{app_version} argument -- an explicit version string supplied by
#'     the caller, e.g. from an \code{app_version()} function bundled with the
#'     app. Used on shinyapps.io where the package is not installed.
#' }
#'
#' @param package_name Name of the package / repository.
#' @param app_version  Optional fallback version string.
#'
#' @return A character string with the version (e.g. "1.1.0"), or NULL if
#'   neither strategy succeeds.
.resolve_app_version <- function(package_name, app_version = NULL) {

  # Strategy 1: installed package
  v <- tryCatch(
    as.character(utils::packageVersion(package_name)),
    error = function(e) NULL
  )
  if (!is.null(v)) {
    message("[version_check] Version source: installed package")
    return(v)
  }

  # Strategy 2: explicit fallback supplied by the caller
  if (!is.null(app_version)) {
    message("[version_check] Version source: app_version() fallback")
    return(as.character(app_version))
  }

  NULL
}


#' Build a shinyWidgets::alert() UI element for the version banner
#'
#' @param current          Current installed version string.
#' @param latest           Latest GitHub release version string, or NULL when
#'   the check could not complete (e.g. no internet).
#' @param update_available Logical; whether latest > current.
#' @param github_user      GitHub username / organisation.
#' @param github_repo      Repository name.
#'
#' @return A \code{shinyWidgets::alert()} UI element.
.build_version_alert <- function(current,
                                 latest,
                                 update_available,
                                 github_user,
                                 github_repo) {

  repo_url <- sprintf(
    "https://github.com/%s/%s/releases/latest",
    github_user,
    github_repo
  )

  if (is.null(latest)) {
    # Version check could not complete (no internet or API failure)
    return(
      shinyWidgets::alert(
        tags$b("Version check failed. "),
        sprintf(
          "Running version %s. Could not reach GitHub to check for updates.",
          current
        ),
        status      = "warning",
        dismissible = TRUE
      )
    )
  }

  if (update_available) {
    shinyWidgets::alert(
      tags$b("Update available! "),
      sprintf(
        "You are running version %s. Version %s is available on GitHub.",
        current, latest
      ),
      " ",
      tags$a(href = repo_url, target = "_blank", "View release"),
      status      = "warning",
      dismissible = TRUE
    )
  } else {
    # Up to date -- only shown in verbose mode
    shinyWidgets::alert(
      tags$b("Up to date. "),
      sprintf(
        "You are running version %s, which matches the latest release (%s).",
        current, latest
      ),
      status      = "success",
      dismissible = TRUE
    )
  }
}


# -- Public API ----------------------------------------------------------------

#' Run a startup version check and return a banner UI element
#'
#' Call this \strong{outside} \code{server()} -- typically in
#' \code{global.R} or at the top of \code{app.R} -- and embed the returned
#' value directly in your UI definition. Because the check runs at startup
#' the banner is present on first render with no loading delay, and no
#' \code{uiOutput()} / \code{renderUI()} wiring is needed.
#'
#' \strong{Normal mode} (\code{verbose = FALSE}): returns a banner only when
#' a newer version is available or when the check fails. Returns \code{NULL}
#' when the app is up to date (Shiny silently ignores \code{NULL} in the UI).
#'
#' \strong{Verbose / debug mode} (\code{verbose = TRUE}): always returns a
#' banner -- including a success banner when up to date -- so you can confirm
#' the check ran and inspect both version strings during development.
#'
#' @param github_user  GitHub username or organisation that owns the repository.
#' @param github_repo  Repository name. Also used as the package name for
#'   \code{utils::packageVersion()}.
#' @param app_version  Optional fallback version string for environments where
#'   the package is not installed (e.g. shinyapps.io). Pass the result of your
#'   \code{app_version()} function here. Ignored when \code{packageVersion()}
#'   succeeds.
#' @param verbose      Logical; if \code{TRUE} a banner is always returned.
#'   Defaults to \code{FALSE}.
#'
#' @return A \code{shinyWidgets::alert()} UI element, or \code{NULL} when there
#'   is nothing to show (up to date in non-verbose mode).
#'
#' @examples
#' \dontrun{
#' # global.R or top of app.R
#' source("version_check.R")
#' version_banner <- check_app_version(
#'   github_user = "my-org",
#'   github_repo = "my-shiny-app",
#'   app_version = app_version()   # fallback for shinyapps.io
#' )
#'
#' # ui.R
#' fluidPage(
#'   version_banner,
#'   # ... rest of UI
#' )
#'
#' # Verbose mode for development
#' version_banner <- check_app_version(
#'   github_user = "my-org",
#'   github_repo = "my-shiny-app",
#'   app_version = app_version(),
#'   verbose     = TRUE
#' )
#' }
check_app_version <- function(github_user,
                              github_repo,
                              app_version = NULL,
                              verbose     = FALSE) {

  # -- 1. Resolve current version ----------------------------------------------
  local_version <- .resolve_app_version(github_repo, app_version)
  if (is.null(local_version)) {
    message(sprintf(
      "[version_check] Could not determine version for '%s' (package not installed and no app_version() fallback supplied).",
      github_repo
    ))
    return(NULL)
  }

  message(sprintf("[version_check] Current version: %s", local_version))

  # -- 2. Internet check -------------------------------------------------------
  if (!.has_internet()) {
    message("[version_check] No internet connection detected -- skipping.")

    if (verbose) {
      return(.build_version_alert(
        current          = local_version,
        latest           = NULL,
        update_available = FALSE,
        github_user      = github_user,
        github_repo      = github_repo
      ))
    }

    return(NULL)
  }

  # -- 3. Fetch latest GitHub release ------------------------------------------
  latest_version <- .get_latest_github_version(github_user, github_repo)
  if (is.null(latest_version)) {
    message("[version_check] Could not retrieve latest version from GitHub.")

    if (verbose) {
      return(.build_version_alert(
        current          = local_version,
        latest           = NULL,
        update_available = FALSE,
        github_user      = github_user,
        github_repo      = github_repo
      ))
    }

    return(NULL)
  }

  message(sprintf("[version_check] Latest GitHub release: %s", latest_version))

  # -- 4. Compare versions -----------------------------------------------------
  update_available <- numeric_version(latest_version) > numeric_version(local_version)

  if (update_available) {
    message(sprintf(
      "[version_check] Update available: %s -> %s",
      local_version, latest_version
    ))
  } else {
    message(sprintf("[version_check] App is up to date (%s).", local_version))
  }

  # -- 5. Return banner --------------------------------------------------------
  # An update was found       -> always return a warning banner
  # Up to date + verbose      -> return a success banner
  # Up to date + not verbose  -> return NULL (Shiny ignores NULL in the UI)
  if (update_available || verbose) {
    .build_version_alert(
      current          = local_version,
      latest           = latest_version,
      update_available = update_available,
      github_user      = github_user,
      github_repo      = github_repo
    )
  } else {
    NULL
  }
}
