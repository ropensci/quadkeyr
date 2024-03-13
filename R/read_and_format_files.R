#' Read all the .csv files in a folder and format the data.
#'
#' @description This function reads all the `.csv` files in a particular folder.
#' These files consistently contain identical columns,
#' with variations only in location, day, and time.
#' As a result,
#' we can uniformly apply specific formatting to columns across these files.
#'
#' @param path_to_csvs Path to the folder where the `.csv` files are stored
#' @param colnames Columns to include in the results (as character).
#' For more information go to [readr::read_csv()]
#' @param coltypes Column specifications (as strings).
#' See vignette("readr", package = "readr") for more details.
#' documentation.
#' @param keep_format Vector of column names, 
#' besides `date_time`, `day` and `quadkey`, that you 
#' don't want to convert to a number.
#'
#' @seealso \code{\link{format_fb_data}}
#' @seealso \code{\link[readr]{read_csv}}
#'
#' @return A data.frame with the information of all the files read.
#' @export
#' @examples
#'
#' files <- read_fb_mobility_files(
#'   path_to_csvs = paste0(system.file("extdata",
#'     package = "quadkeyr"
#'   ), "/"),
#'   colnames = c( # The columns not listed here will be omitted
#'     "lat",
#'     "lon",
#'     "quadkey",
#'     "date_time",
#'     "n_crisis",
#'     "percent_change",
#'     "day",
#'     "hour"
#'   ),
#'   coltypes = list(
#'     lat = "d",
#'     lon = "d",
#'     quadkey = "c",
#'     date_time = "T",
#'     n_crisis = "c",
#'     percent_change = "c",
#'     day = "D",
#'     hour = "i"
#'   )
#' )
#'
#' head(files)
read_fb_mobility_files <- function(path_to_csvs,
                                   colnames,
                                   coltypes,
                                   keep_format = NULL) {

  # This data always have the same format
  fnames <- list.files(
    path = path_to_csvs,
    pattern = "\\.csv$"
  )

  # Give an error if there are not empty files
  if (length(fnames) == 0) {
    stop("Error:.csv files not found in the specified directory.")
  }

  fnames <- paste0(
    path_to_csvs,
    fnames
  )

  # There could be empty files, let's remove them
  fnames <- fnames[file.info(fnames)$size != 0]

  data <- purrr::map_dfr(
    .x = fnames,
    .f = function(files) {
      readr::read_csv(
        files,
        # Let's select the columns given by the user.
        col_select = dplyr::all_of(colnames),
        col_names = TRUE, # header
        col_types = coltypes
      )
    }
  )

  data <- format_fb_data(data,
                         keep_format = keep_format)

  if (nrow(missing_combinations(data)) > 0) {
    message(paste(
      "The files with the following combinations of",
      "days and times are not present or have 0KB"
    ))
    print(missing_combinations(data))
  }
  data
}


#' Format the Facebook mobility data
#'
#' @description This function removes unnecessary characters such as `\\N`
#' and ensures that the format of the date and QuadKeys is correct.
#'
#' @param data A data.frame with a `quadkey` and `date_time` columns
#' and other variables
#' @param keep_format Vector of column names, 
#' besides `date_time`, `day` and `quadkey`, that you 
#' don't want to convert to a number.
#'
#' @return A data.frame without `\N`, 
#' `quadkey` without scientific notation and
#' a new column `day` and `hour`
#' 
#' @export
#'
#' @seealso \code{\link{read_fb_mobility_files}}
#'
#' @examples
#'
#' data(result_read_fb_mobility_data)
#' format_fb_data(data = result_read_fb_mobility_data)
format_fb_data <- function(data,
                           keep_format = NULL) {

  # remove scientific notation
  data$quadkey <- format(data$quadkey,
    scientific = FALSE
  )

  # change date format
  data$day <- lubridate::date(data$date_time)

  # get the hour
  data$hour <- as.numeric(format(as.POSIXct(data$date_time,
    format = "%Y-%m-%d %H%M"
  ),
  format = "%H"
  ))

  # replace \\N with NA
  data <- data |>
    dplyr::mutate(dplyr::across(
      -c("date_time", "day"), # tidyselect
      ~ ifelse(. == "\\N", NA, .)
    )) |>
    dplyr::mutate(dplyr::across(
      -c("date_time", "day", "quadkey", 
         dplyr::all_of(keep_format)), # tidyselect
      as.numeric
    ))

  data
}

#' Detect dates and hours missing in filenames
#'
#' @description Facebook mobility data is reported daily at 3 different hours
#' (0, 8, 16).
#' This function reads the data extracted from the current files and detects
#' if any file for a particular day or hour is missing.
#'
#' @param data A data.frame with a `day` and hour column.
#'
#' @importFrom rlang .data
#'
#' @return A data.frame with the missing days and hours, if any.
#' @export
#' @examples
#'
#' # Sample dataset
#' data <- data.frame(
#'   country = c("US", "MX", "MX"), 
#'   day = c("2023-01-01", "2023-01-03", "2023-01-05"),
#'   hour = c(0, 8, 16)
#' )
#'
#' missing_combinations(data)
missing_combinations <- function(data) {
  data <- data |>
    dplyr::mutate(day = as.Date(.data$day))

  # Generate all combinations of days and times
  all_combinations <- expand.grid(
    day = seq(
      from = min(data$day),
      to = max(data$day),
      by = "days"
    ),
    hour = c(0, 8, 16)
  )

  # Select the dates not present on the dataset
  missing_combinations <- dplyr::anti_join(all_combinations,
    data,
    by = c("day", "hour")
  )
  return(missing_combinations)
}
