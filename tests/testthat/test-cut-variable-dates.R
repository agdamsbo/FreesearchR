test_that("datetime cutting works", {
  ## HMS
  data <- readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20"))

  breaks <- list(2, "min", "hour", hms::as_hms(c("01:00:00", "03:01:20", "9:20:20")))

  lapply(breaks, \(.x){
    cut_var(x = data, breaks = .x)
  }) |> expect_snapshot()


  data <- readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA))

  lapply(breaks, \(.x){
    cut_var(x = data, breaks = .x)
  }) |> expect_snapshot()

  expect_snapshot(
    readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA)) |> cut_var(breaks = lubridate::as_datetime(c(hms::as_hms(levels(cut_var(data, 2))), hms::as_hms(max(data, na.rm = TRUE) + 1))), right = FALSE)
  )

  ## DATETIME

  data <- readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20"))

  breaks <- list(list(breaks = 2), list(breaks = "weekday"), list(breaks = "month_only"), list(breaks = NULL, format = "%A-%H"))

  lapply(breaks, \(.x){
    do.call(cut_var, modifyList(.x, list(x = data)))
  }) |> expect_snapshot()
})

## is_any_class
test_that("is_any_class works", {
  expect_snapshot(
    vapply(REDCapCAST::redcapcast_data, \(.x){
    is_any_class(.x, c("hms", "Date", "POSIXct", "POSIXt"))
  }, logical(1))
  )

  expect_snapshot(
  vapply(REDCapCAST::redcapcast_data, is_datetime, logical(1))

  )

})

