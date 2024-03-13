#' Create and save raster images for different dates and times
#'
#' @description Creates one raster by each date and time reported and
#'  saves it as a `.tif`.
#'
#' @param template A `sf` POLYGON data.frame
#' @param nx Integer; number of cells in x direction.
#' @param ny Integer; number of cells in y direction.
#' @param data A `sf` POLYGON data.frame with the variable we want to represent
#' in the raster.
#' @param var The column name of the variable to plot.
#' @param filename Select a name for the file. The date and time will
#'  be included automatically in the name.
#' @param path Path where the files should be stored.
#'
#' @seealso \code{\link{st_as_stars}}, \code{\link{st_rasterize}}
#' @seealso \code{\link{missing_combinations}}
#'
#' @importFrom rlang .data
#'
#' @return as many .tif files as dates and times in the dataset
#' @export
#' @examples
#' files <- read_fb_mobility_files(
#'   path_to_csvs = paste0(system.file("extdata",
#'     package = "quadkeyr"
#'   ), "/"),
#'   colnames = c(
#'     "lat", "lon",
#'     "quadkey", "date_time",
#'     "n_crisis", "percent_change"
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
#' # Get a regular grid and create the polygons
#' regular_grid <- get_regular_polygon_grid(data = files)
#'
#' # Keep only the QuadKeys reported
#' files_polygons <- files |>
#'   dplyr::inner_join(regular_grid$data,
#'     by = c("quadkey")
#'   )
#'
#' \dontrun{
#' # Generate the raster files
#' polygon_to_raster(
#'   data = files_polygons,
#'   nx = regular_grid$num_cols,
#'   ny = regular_grid$num_rows,
#'   template = files_polygons,
#'   var = "percent_change",
#'   filename = "cityA",
#'   path = paste0(
#'     system.file("extdata",
#'       package = "quadkeyr"
#'     ),
#'     "/"
#'   )
#' )
#' }
polygon_to_raster <- function(data,
                              nx, ny,
                              template,
                              var = "percent_change",
                              filename,
                              path) {

  # detect and reposrt combination of dates and times missing
  if (nrow(missing_combinations(data)) > 0) {
    warning("These combinations of days and times are missing in the dataset")
    print(missing_combinations(data))
  }


  days <- seq(
    from = min(data$day),
    to = max(data$day),
    by = "days"
  )

  mc <- missing_combinations(data)

  for (i in days) {
    for (p in c(0, 8, 16)) {

      # Is this iteration a missing file?
      no_data <- mc |>
        dplyr::filter(.data$day == as.Date(i,
          origin = "1970-01-01"
        ) &
          .data$hour == p)

      # If it is, skip this iteration
      if (nrow(no_data) > 0) {
        next
      }

      data_raster <- data |>
        dplyr::filter(.data$day == as.Date(i,
          origin = "1970-01-01"
        ) &
          .data$hour == p)

      # Create raster using a template to avoid errors
      file <- create_stars_raster(
        template = template,
        nx = nx,
        ny = ny,
        data = data_raster,
        var = var
      )

      # Save raster file
      stars::write_stars(
        obj = file,
        dsn = paste0(
          path,
          filename,
          "_",
          as.Date(i, origin = "1970-01-01"),
          "_",
          p,
          ".tif"
        )
      )
    }
  }
}
