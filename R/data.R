#' data_provided: fake dataset
#'
#' A data frame simulating a variable asociated with QuadKey numbers.
#'
#' @format ## `data_provided`
#' A data frame with 360 rows and 2 columns:
#' \describe{
#'   \item{quadkey}{QuadKey number as character}
#'   \item{variable}{Numeric variable}
#' }
"data_provided"

#' result_read_all_files: Fake dataset
#'
#' A data frame similar to a the potential output of read_all_files().
#' 
#' @format ## `result_read_all_files`
#' A data frame with 134,492 rows and 9 columns:
#' \describe{
#'   \item{lat}{Latitude of QuadKey centroid}
#'   \item{lon}{Longitude of QuadKey centroid}
#'   \item{quadkey}{QuadKey number as character}
#'   \item{country}{Country name}
#'   \item{date_time}{Date in format %Y-%m-%d %H%M}
#'   \item{n_crisis}{Variable}
#'   \item{percent_change}{Variable}
#'   \item{day}{Day in format %Y-%m-%d}
#'   \item{time}{Hour of the day as a number between 1 and 24}
#'   }
"result_read_all_files"