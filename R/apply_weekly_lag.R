#' Apply a 7 day lag to the variable n_crisis
#'
#' @description Applying a week lag to the data will create rasters showing how
#' people was moving a week ago the date of interest. This function works only
#' for quadkeys without NAs.
#'
#' @param data A dataset
#'
#' @return A dataframe with the extra colums n_crisis_lag_7 and n_crisis_percent_7
#' @export
#'
#' @examples
#'
#' data <- result_read_all_files
#' apply_weekly_lag(data)
#'
apply_weekly_lag <- function(data){

out_data <- c()

for(i in unique(data$quadkey)){

  inter <-  data |>
             dplyr::filter(quadkey == i)

 # I am only considering cases where there have not been NAs
  if(!is.na(sum(inter$n_crisis))){

    quadkey_lag <- inter |>
      dplyr::group_by(quadkey, time) |>
      dplyr::mutate(n_crisis_lag_7 = dplyr::lag(as.numeric(n_crisis), n = 7)) |>
      dplyr::mutate(percent_change_7 = ((n_crisis_lag_7 - n_crisis)/n_crisis) * 100)

    out_data <- rbind(out_data, quadkey_lag)

  }
}

return(out_data)
}
