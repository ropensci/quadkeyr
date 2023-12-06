#' Read all the .csv files in a folder and format the data.
#'
#' @description This function reads all the .csv files in a particular folder.
#' These files consistently contain identical columns, with variations
#' only in location, day, and time. As a result, we can uniformly apply
#' specific formatting to columns across these files."
#'
#' @param path_to_csvs Path to the folder where the .csv files are stored
#' @param colnames Columns to include in the results (as character).
#' For more information go to readr::read_csv() 
#' documentation.
#'  
#' @seealso \code{\link{format_data}}
#' @seealso \code{\link[readr]{read_csv}}
#'
#' @return A dataframe with the information of all the files.
#' @export
#'
#' @examples
#'
#' 
#' # read_all_files(path_to_csvs = 'data/',
#' #                 colnames = c("lat", "lon", "quadkey", "date_time", 
#' #                              "n_crisis", "percent_change"),
#' #                 coltypes = "dddTcc")
#'
read_all_files <- function(path_to_csvs, 
                           colnames){

  

  # This data always have the same format
  fnames <- list.files(path = path_to_csvs,
                       pattern = "\\.csv$")

  fnames <- paste0(path_to_csvs,
                   fnames)
  
  # There could be empty files, let's remove them
  fnames <- fnames[file.info(fnames)$size != 0]

  data <- purrr::map_dfr(fnames,
                     readr::read_csv,
                     col_select = dplyr::all_of(colnames), # tidyselect
                     col_names = TRUE, # header
                     show_col_types = FALSE) # don't print messages


  data <- format_data(data)

  if (nrow(missing_combinations(data)) > 0) {
    message(paste("The files with the following combinations of",
                  "days and times are not present or have 0KB"))
    print(missing_combinations(data))
  }else{
    message("There aren't missing dates or times")
  }

  data

}


#' Format the data
#'
#' @description This function modifies the format of three columns the provided
#' data.
#'
#' @param data A dataframe with a quadkey, date_time, country columns and 
#' other numeric variables
#'
#' @return A dataframe.
#' @export
#'
#' @seealso \code{\link{read_all_files}}
#'
#' @examples
#'
#' #data(onefile)
#' #format_data(data = onefile)
#'
format_data <- function(data){

  # remove scientific notation
  data$quadkey <- format(data$quadkey,
                         scientific = FALSE)

  # change date format
  data$day <- lubridate::date(data$date_time)

  data$time <- as.numeric(format(as.POSIXct(data$date_time, 
                                            format = "%Y-%m-%d %H%M"), 
                                 format = "%H"))

  # replace \\N with NA
  data <- data |>
    dplyr::mutate(dplyr::across(-c("date_time", "day"), #tidyselect
                  ~ ifelse(. == "\\N", NA, .))) |> 
    dplyr::mutate(dplyr::across(-c("date_time", "day", "quadkey"),
               as.numeric))
     
  data
}

#' Detects dates and times missing
#'
#' @description Facebook mobility data is reported daily at 3 different times
#' (0, 8, 16).
#' This function reads the data extracted from the current files and detects
#' if any day or time is missing.
#'
#' @param data A dataframe with a day and time column.
#'
#' @return A dataframe with the missing days and times, if any.
#' @export
#'
#' @examples
#'
#' # Sample dataset
#' data <- data.frame(
#'  day = c("2023-01-01", "2023-01-03", "2023-01-05"),
#'  time = c(0, 8, 16)
#' )
#'
#' missing_combinations(data)
#'
missing_combinations <- function(data) {

  data[, "day"] <- as.Date(data[, "day"])
  
  # Generate all combinations of days and times
  all_combinations <- expand.grid(
    day = seq(from = min(data$day),
              to = max(data$day),
              by = 'days'),
    time = c(0, 8, 16)
  )

  # Select the dates not present on the dataset
  missing_combinations <- dplyr::anti_join(all_combinations,
                                    data,
                                    by = c("day", "time"))

  return(missing_combinations)
}


