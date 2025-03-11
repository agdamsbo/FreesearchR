#' Alternative pivoting method for easily pivoting based on name pattern
#'
#' @description
#' This function requires and assumes a systematic naming of variables.
#' For now only supports one level pivoting. Adding more levels would require
#' an added "ignore" string pattern or similarly. Example 2.
#'
#'
#' @param data data
#' @param pattern pattern(s) to match. Character vector of length 1 or more.
#' @param type type of match. can be one of "prefix","infix" or "suffix".
#' @param id.col ID column. Will fill ID for all. Column name or numeric index.
#' Default is "1", first column.
#' @param instance.name
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' data.frame(
#'   1:20, sample(70:80, 20, TRUE),
#'   sample(70:100, 20, TRUE),
#'   sample(70:100, 20, TRUE),
#'   sample(170:200, 20, TRUE)
#' ) |>
#'   setNames(c("id", "age", "weight_0", "weight_1", "height_1")) |>
#'   wide2long(pattern = c("_0", "_1"), type = "suffix")
#' data.frame(
#'   1:20, sample(70:80, 20, TRUE),
#'   sample(70:100, 20, TRUE),
#'   sample(70:100, 20, TRUE),
#'   sample(170:200, 20, TRUE)
#' ) |>
#'   setNames(c("id", "age", "weight_0", "weight_a_1", "height_b_1")) |>
#'   wide2long(pattern = c("_0", "_1"), type = "suffix")
#' # Optional filling of missing values by last observation carried forward
#' # Needed for mmrm analyses
#' long_missings |>
#'   # Fills record ID assuming none are missing
#'   tidyr::fill(record_id) |>
#'   # Grouping by ID for the last step
#'   dplyr::group_by(record_id) |>
#'   # Filling missing data by ID
#'   tidyr::fill(names(long_missings)[!names(long_missings) %in% new_names]) |>
#'   # Remove grouping
#'   dplyr::ungroup()
wide2long <- function(
    data,
    pattern,
    type = c("prefix", "infix", "suffix"),
    id.col = 1,
    instance.name = "instance") {
  type <- match.arg(type)

  ## Give the unique suffix names to use for identifying repeated measures
  # suffixes <- c("_0", "_1")

  ## If no ID column is present, one is added
  if (id.col == "none" | is.null(id.col)) {
    data <- stats::setNames(
      data.frame(seq_len(nrow(data)), data),
      make.names(c("id", names(data)), unique = TRUE)
    )
    id.col <- 1
  }
# browser()
  ## Relevant columns are determined based on suffixes
  cols <- names(data)[grepl_fix(names(data), pattern = pattern, type = type)]

  ## New colnames are created by removing suffixes
  new_names <- unique(gsub(paste(pattern, collapse = "|"), "", cols))

  out <- split(data, seq_len(nrow(data))) |> # Splits dataset by row
    # Starts data modifications for each subject
    lapply(\(.x){
      ## Pivots data with repeated measures as determined by the defined suffixes
      long_ls <- split.default(
        # Subset only repeated data
        .x[cols],
        # ... and split by meassure
        gsub(paste(new_names, collapse = "|"), "", cols)
      ) |>
        # Sort data by order of given suffixes to ensure chronology
        sort_by(pattern) |>
        # New colnames are applied
        lapply(\(.y){
          setNames(
            .y,
            gsub(paste(pattern, collapse = "|"), "", names(.y))
          )
        })

      # Subsets non-pivotted data (this is assumed to belong to same )
      single <- .x[-match(cols, names(.x))]

      # Extends with empty rows to get same dimensions as long data
      single[(nrow(single) + 1):length(long_ls), ] <- NA

      # Fills ID col
      single[id.col] <- single[1, id.col]

      # Everything is merged together
      merged <- dplyr::bind_cols(
        single,
        # Instance names are defined as suffixes without leading non-characters
        REDCapCAST::as_factor(data.frame(gsub(
          "^[^[:alnum:]]+", "",
          names(long_ls)
        ))),
        dplyr::bind_rows(long_ls)
      )

      # Ensure unique new names based on supplied
      colnames(merged) <- make.names(
        c(
          names(single),
          instance.name,
          names(merged)[(NCOL(single) + 2):NCOL(merged)]
        ),
        unique = TRUE
      )

      merged
    }) |> dplyr::bind_rows()

  rownames(out) <- NULL

  out
}


#' Matches pattern to vector based on match type
#'
#' @param data vector
#' @param pattern pattern(s) to match. Character vector of length 1 or more.
#' @param type type of match. can be one of "prefix","infix" or "suffix".
#'
#' @returns logical vector
#' @export
#'
#' @examples
#' c("id", "age", "weight_0", "weight_1") |> grepl_fix(pattern = c("_0", "_1"), type = "suffix")
grepl_fix <- function(data, pattern, type = c("prefix", "infix", "suffix")) {
  type <- match.arg(type)

  if (type == "prefix") {
    grepl(paste0("^(", paste(pattern, collapse = "|"), ")*"), data)
  } else if (type == "suffix") {
    grepl(paste0("*(", paste(pattern, collapse = "|"), ")$"), data)
  } else if (type == "infix") {
    grepl(paste0("*(", paste(pattern, collapse = "|"), ")*"), data)
  }
}
