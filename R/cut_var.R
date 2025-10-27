#' Extended cutting function with fall-back to the native base::cut
#'
#' @param x an object inheriting from class "hms"
#' @param ... passed on
#'
#' @export
#' @name cut_var
cut_var <- function(x, ...) {
  UseMethod("cut_var")
}

#' @export
#' @name cut_var
cut_var.default <- function(x, ...) {
  base::cut(x, ...)
}

#' @name cut_var
#'
#' @return factor
#' @export
#'
#' @examples
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var("min")
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(breaks = "hour")
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(breaks = hms::as_hms(c("01:00:00", "03:01:20", "9:20:20")))
#' d_t <- readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA))
#' f <- d_t |> cut_var(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA)) |> cut_var(breaks = lubridate::as_datetime(c(hms::as_hms(levels(f)), hms::as_hms(max(d_t, na.rm = TRUE) + 1))), right = FALSE)
cut_var.hms <- function(x, breaks, ...) {
  ## as_hms keeps returning warnings on tz(); ignored
  suppressWarnings({
    if (hms::is_hms(breaks)) {
      breaks <- lubridate::as_datetime(breaks)
    }
    x <- lubridate::as_datetime(x)
    out <- cut_var.POSIXt(x, breaks = breaks, ...)
    attr(out, which = "brks") <- hms::as_hms(lubridate::as_datetime(attr(out, which = "brks")))
    attr(out, which = "levels") <- as.character(hms::as_hms(lubridate::as_datetime(attr(out, which = "levels"))))
  })
  out
}

#' @name cut_var
#' @param x an object inheriting from class "POSIXt" or "Date"
#'
#' @examples
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(2)
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "weekday")
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "month_only")
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = NULL, format = "%A-%H")
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = NULL, format = "%W")
cut_var.POSIXt <- function(x, breaks, right = FALSE, include.lowest = TRUE, start.on.monday = TRUE, ...) {
  breaks_o <- breaks
  args <- list(...)
  # browser()
  if (is.numeric(breaks)) {
    breaks <- quantile(
      x,
      probs = seq(0, 1, 1 / breaks),
      right = right,
      include.lowest = include.lowest,
      na.rm = TRUE
    )
  }

  if ("format" %in% names(args)) {
    assertthat::assert_that(is.character(args$format))
    out <- forcats::as_factor(format(x, format = args$format))
  } else if (identical(breaks, "weekday")) {
    ## This is
    ds <- as.Date(1:7) |>
      (\(.x){
        sort_by(format(.x, "%A"), as.numeric(format(.x, "%w")))
      })()

    if (start.on.monday) {
      ds <- ds[c(7, 1:6)]
    }
    out <- factor(weekdays(x), levels = ds) |> forcats::fct_drop()
  } else if (identical(breaks, "month_only")) {
    ## Simplest way to create a vector of all months in order
    ## which will also follow the locale of the machine
    ms <- paste0("1970-", 1:12, "-01") |>
      as.Date() |>
      months()

    out <- factor(months(x), levels = ms) |> forcats::fct_drop()
  } else {
    ## Doesn't really work very well for breaks other than the special character cases as right border is excluded
    out <- base::cut.POSIXt(x, breaks = breaks, right = right, ...) |> forcats::fct_drop()
    # browser()
  }
  l <- levels(out)
  if (is.numeric(breaks_o)) {
    l <- breaks
  } else if (is.character(breaks) && length(breaks) == 1 && !(identical(breaks, "weekday") | identical(breaks, "month_only"))) {
    if (include.lowest) {
      if (right) {
        l <- c(l, min(as.character(x)))
      } else {
        l <- c(l, max(as.character(x)))
      }
    }
  } else if (length(l) < length(breaks_o)) {
    l <- breaks_o
  }

  attr(out, which = "brks") <- l
  out
}

#' @name cut_var
#' @param x an object inheriting from class "POSIXct"
cut_var.POSIXct <- cut_var.POSIXt

#' @name cut_var
#' @param x an object inheriting from class "POSIXct"
#'
#' @examples
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(2)
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "weekday")
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(format = "%W")
cut_var.Date <- function(x, breaks = NULL, start.on.monday = TRUE, ...) {
  args <- list(...)

  if ("format" %in% names(args)) {
    assertthat::assert_that(is.character(args$format))
    out <- forcats::as_factor(format(x, format = args$format))
  } else if (identical(breaks, "weekday")) {
    ds <- as.Date(1:7) |>
      (\(.x){
        sort_by(format(.x, "%A"), as.numeric(format(.x, "%w")))
      })()

    if (start.on.monday) {
      ds <- ds[c(7, 1:6)]
    }
    out <- factor(weekdays(x), levels = ds) |> forcats::fct_drop()
  } else if (identical(breaks, "month_only")) {
    ms <- paste0("1970-", 1:12, "-01") |>
      as.Date() |>
      months()

    out <- factor(months(x), levels = ms) |> forcats::fct_drop()
  } else {
    ## Doesn't really work very well for breaks other than the special character cases as right border is excluded
    out <- base::cut.Date(x, breaks = breaks, ...) |> forcats::fct_drop()
    # browser()
  }
  out
}


#' Simplify a factor to only the top or bottom n levels
#'
#' @param type
#'
#' @name cut_var
#'
#' @returns factor
#' @export
#'
#' @examples
#' mtcars$carb |>
#'   as.factor() |>
#'   cut_var(2) |>
#'   table()
#'
#' mtcars$carb |>
#'   as.factor() |>
#'   cut_var(20, "bottom") |>
#'   table()
cut_var.factor <- function(x, breaks = NULL, type = c("top", "bottom"), other = "Other", ...) {
  args <- list(...)

  if (is.null(breaks)) {
    return(x)
  }

  type <- match.arg(type)

  tbl <- sort(table(x), decreasing = TRUE)

  if (type == "top") {
    lvls <- names(tbl[seq_len(breaks)])
  } else if (type == "bottom") {
    lvls <- names(tbl)[!tbl / NROW(x) * 100 < breaks]
  }

  if (other %in% lvls) {
    other <- paste(other, "_freesearchr")
  }

  ## Relabel and relevel
  out <- forcats::fct_relabel(
    x,
    \(.x){
      ifelse(.x %in% lvls, .x, other)
    }
  ) |>
    forcats::fct_relevel(lvls, other)

  attr(out, which = "brks") <- breaks
  out
}


#' Test class
#'
#' @param data data
#' @param class.vec vector of class names to test
#'
#' @return factor
#' @export
#'
#' @examples
#' \dontrun{
#' vapply(REDCapCAST::redcapcast_data, \(.x){
#'   is_any_class(.x, c("hms", "Date", "POSIXct", "POSIXt"))
#' }, logical(1))
#' }
is_any_class <- function(data, class.vec) {
  any(class(data) %in% class.vec)
}

#' Test is date/datetime/time
#'
#' @param data data
#'
#' @return factor
#' @export
#'
#' @examples
#' vapply(REDCapCAST::redcapcast_data, is_datetime, logical(1))
is_datetime <- function(data) {
  is_any_class(data, class.vec = c("hms", "Date", "POSIXct", "POSIXt"))
}
