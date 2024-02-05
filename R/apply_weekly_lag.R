#' Apply a 7 day lag to the variable n_crisis
#'
#' @description Applying a week lag to the data will create raster images
#' showing the mobility a week before the date of interest.
#' This function works only for quadkeys without NAs.
#'
#' @param data A dataset
#'
#' @importFrom rlang .data
#'
#' @return A dataframe with the extra columns n_crisis_lag_7 and
#' percent_change_7.
#'
#' n_crisis_lag_7, is the same variable defined as n_crisis in the Facebook
#' dataset with a 7 day lag applied.
#'
#' percent_change_7 is the difference between the n_crisis value between weeks
#' expressed as percentage.
#'
#'
#' @export
#'
#' @examples
#'
#' files <- read_fb_mobility_files(path_to_csvs = paste0(system.file("extdata",
#'                                 package = "quadkeyr"), "/"),
#'                                 colnames = c("lat", "lon", 
#'                                              "quadkey", "date_time", 
#'                                              "n_crisis", "percent_change"),
#'                                 coltypes = list(
#'                                                 lat = 'd',
#'                                                 lon = 'd',
#'                                                 quadkey = 'c',
#'                                                 date_time = 'T',
#'                                                 n_crisis = 'c',
#'                                                 percent_change = 'c')) 
#'  
#' apply_weekly_lag(files)
#'
apply_weekly_lag <- function(data) {
  out_data <- c()

  for (i in unique(data$quadkey)) {
    inter <- data |>
      dplyr::filter(.data$quadkey == i)

    # I am only considering cases where there aren't NAs
    if (!is.na(sum(inter$n_crisis))) {
      quadkey_lag <- inter |>
        dplyr::group_by(.data$quadkey, .data$hour) |>
        dplyr::mutate(n_crisis_lag_7 = dplyr::lag(as.numeric(.data$n_crisis),
          n = 7
        )) |>
        dplyr::mutate(percent_change_7 = ((.data$n_crisis_lag_7 - .data$n_crisis) /
          .data$n_crisis) * 100)

      out_data <- rbind(out_data, quadkey_lag)
    }
  }

  return(out_data)
}
