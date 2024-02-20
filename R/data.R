#' QuadKey-identified Dataset
#'
#' A data.frame simulating a QuadKey-identified dataset.
#' @keywords internal
#' @format ## `data_provided`
#' A data.frame with 360 rows and 2 columns:
#' \describe{
#'   \item{quadkey}{QuadKey as a string}
#'   \item{variable}{Numeric variable}
#' }
"data_provided"

#' Dataset with (fake) Facebook mobility data
#'
#' A data.frame similar to a the potential output of `read_fb_mobility_files`.
#' @keywords internal
#' @format ## `result_read_fb_mobility_data`
#' A data frame with 134,492 rows and 9 columns:
#' \describe{
#'   \item{lat}{Latitude of the QuadKey centroid}
#'   \item{lon}{Longitude of the QuadKey centroid}
#'   \item{quadkey}{QuadKey as a string}
#'   \item{country}{Country name}
#'   \item{date_time}{Date in format %Y-%m-%d %H%M}
#'   \item{n_crisis}{Variable}
#'   \item{percent_change}{Variable}
#'   \item{day}{Day in format %Y-%m-%d}
#'   \item{hour}{Hour of the day as a number between 1 and 24}
#'   }
"result_read_fb_mobility_data"
