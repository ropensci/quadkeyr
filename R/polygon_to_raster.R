#' Create and save raster images for different dates and times
#'
#' @description Creates one raster by each date and time reported and saves it as a .tif.
#'
#' @param template A spatial dataset (sf) with the polygon grid used as template
#' @param nx Integer; number of cells in x direction.
#' @param ny Integer; number of cells in y direction.
#' @param data A spatial dataframe (sf) with the variable we want to represent in the raster.
#' @param variable The column name of the variable to plot.
#' @param filename Select a name for the file. The date and time will be included automatically in the name.
#' @param path Path where the files should be stored.
#'
#' @seealso \code{\link{st_as_stars}}, \code{\link{st_rasterize}}
#' @seealso \code{\link{missing_combinations}}
#'
#' @return as many .tif files as dates and times in the dataset
#' @export
#'
#' @examples
#'
#' # TBD
#'
polygon_to_raster <- function(data,
                              nx, ny,
                              template,
                              variable = 'percent_change',
                              filename,
                              path){

  if (nrow(missing_combinations(data)) > 0) {
    warning("These combinations of days and times are missing in the dataset")
    missing_combinations(data)
  }


  days <- seq(from = min(data$day),
              to = max(data$day),
              by = 'days')

  mc = missing_combinations(data)

for(i in days){

for(p in c(0, 8, 16)){

  # is this iteration a missing file?
  no_data <- mc |>
            dplyr::filter(.data$day ==  as.Date(i, origin = "1970-01-01") & .data$time == p)

  # if it is, skip it
  if (nrow(no_data) > 0) {
    next  # Skip this iteration
  }


  data <- data |>
            dplyr::filter(.data$day ==  as.Date(i, origin = "1970-01-01") & .data$time == p)

  file <-  create_raster(template = template,
                nx = nx,
                ny = ny,
                data = data,
                var = variable )

          stars::write_stars(obj = file,
                             dsn = paste0(path, filename,
                                            "_",
                                   as.Date(i, origin = "1970-01-01"),
                                            "_",
                                             p,
                                    ".tif"))

}}
}



