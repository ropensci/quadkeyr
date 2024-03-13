#' Apply a 7 day lag to the variable `n_crisis`
#'
#' @description Applying a week lag to the data will create raster images
#' showing the mobility a week before the date of interest.
#' This function works only for QuadKeys reported without NAs for
#' `n_crisis` and `percent_change` variables .
#'
#' @param data A data.frame with the columns `quadkey`,
#' `day`, `hour` and `n_crisis`.
#'
#' @importFrom rlang .data
#'
#' @return A data.frame with the extra columns `n_crisis_lag_7` and
#' `percent_change_7`.
#'
#' * `n_crisis_lag_7`, is the same variable defined as `n_crisis`
#' in the Facebook mobility data.frame with a 7 day lag applied.
#'
#' * `percent_change_7` is the difference between
#' the `n_crisis` value between weeks expressed as percentage.
#'
#' @export
#' @examples
#'
#' files <- read_fb_mobility_files(
#'   path_to_csvs = paste0(system.file("extdata",
#'     package = "quadkeyr"
#'   ), "/"),
#'   colnames = c(
#'     "lat",
#'     "lon",
#'     "quadkey",
#'     "date_time",
#'     "n_crisis",
#'     "percent_change"
#'   ),
#'   coltypes = list(
#'     lat = "d",
#'     lon = "d",
#'     quadkey = "c",
#'     date_time = "T",
#'     n_crisis = "c",
#'     percent_change = "c"
#'   )
#' )
#'
#' apply_weekly_lag(data = files)
apply_weekly_lag <- function(data) {
  # First I must check that we have all the days and months
  if (nrow(missing_combinations(data)) != 0) {
    mc <- missing_combinations(data)
    # create the combination of QuadKeys,
    # days and hours missing in a grid
    missing_data <- expand.grid(
      quadkey = unique(data$quadkey),
      day = mc$day,
      hour = mc$hour
    )
    
    # Add the missing data to the original files
    # Now I have quadkey, day and hour columns complete
    data <- dplyr::bind_rows(data, missing_data) |>
      dplyr::arrange(.data$day, .data$hour)
  }
  
  # I am only considering cases where there aren't NAs
  # Let's remove the QuadKeys with 100% NAs for n_crisis
  qk_data_without_NA  <-  data |>
    dplyr::group_by(.data$quadkey) |>
    dplyr::summarise(empty = !is.na(sum(.data$n_crisis))) |>
    dplyr::filter(.data$empty == FALSE) |>
    dplyr::ungroup()
  
  data <- data |>
    dplyr::filter(.data$quadkey %in% qk_data_without_NA$quadkey)
  
  # QuadKey that appears in all the combination of possible days and hours
  # should occur `qk_rep` times.
  # If a QuadKey is reported fewer times than that, we will remove it
  # to avoid discontinuous sequences of days and subsequent gaps.
  min_date <-  min(data$day)
  max_date <- max(data$day)
  days <- as.numeric(max_date - min_date)
  qk_rep <- (days + 1) * 3
  
  qk_reg <- data |>
    dplyr::count(.data$quadkey) |>
    dplyr::filter(.data$n == qk_rep)
  
  data <- data |>
    dplyr::filter(.data$quadkey %in% qk_reg$quadkey)
  
  # Now that this is all sorted,
  # let's create the lag column
  quadkey_lag <- data |>
    dplyr::group_by(.data$quadkey) |>
    dplyr::arrange(.data$day, .data$hour, .by_group = TRUE) |>
    dplyr::mutate(n_crisis_lag_7 = dplyr::lag(.data$n_crisis,
                               n = (7 * 3))) |>
    dplyr::mutate(percent_change_7 = ((.data$n_crisis_lag_7 - .data$n_crisis) /
                                        .data$n_crisis) * 100)
}



