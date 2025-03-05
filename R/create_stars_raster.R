#' Create a `stars` raster
#'
#' @description The use of a template enables the creation of an accurate
#' raster, even in the presence of NAs.
#'
#' @param template A `sf` POLYGON data.frame to use as template.
#' Check [stars::st_as_stars()] documentation for more details.
#' @param nx Integer; number of cells in x direction.
#' @param ny Integer; number of cells in y direction.
#' @param data A  `sf` POLYGON data.frame with the variable
#' we want to represent in the raster.
#' @param var The column name of the variable to plot.
#'
#' @seealso \code{\link[stars]{st_as_stars}}, \code{\link[stars]{st_rasterize}}
#'
#' @return A `stars` object.
#' @export
#' @examples
#' \dontrun{
#' # Basic workflow
#' # Read the data
#' path <- paste0(
#'   system.file("extdata", package = "quadkeyr"),
#'   "/cityA_2020_04_15_0000.csv"
#' )
#' data <- read.csv(path)
#' data <- format_fb_data(data)
#'
#' complete_polygon_grid <- add_regular_polygon_grid(data = data)
#'
#' stars_object <- create_stars_raster(
#'   data = complete_polygon_grid$data,
#'   template = complete_polygon_grid$data,
#'   var = "percent_change",
#'   nx = complete_polygon_grid$num_cols,
#'   ny = complete_polygon_grid$num_rows
#' )
#' stars_object
#'
#' # Other workflow
#' grid <- create_qk_grid(
#'   xmin = -59,
#'   xmax = -57,
#'   ymin = -35,
#'   ymax = -34,
#'   zoom = 12
#' )
#'
#' grid_coords <- get_qk_coord(data = grid$data)
#'
#' polygrid <- grid_to_polygon(grid_coords)
#'
#' data("data_provided")
#' data_raster <- polygrid |>
#'   dplyr::inner_join(data_provided,
#'     by = c("quadkey")
#'   )
#'
#' raster <- create_stars_raster(
#'   template = data_raster,
#'   nx = grid$num_cols,
#'   ny = grid$num_rows,
#'   data = data_raster,
#'   var = "variable"
#' )
#' }
create_stars_raster <- function(template,
                                nx, ny,
                                data,
                                var) {
  # data should be in sf format
  data_sf <- sf::st_sf(data)
  # create raster template
  raster_tmplt <-
    stars::st_as_stars(
      sf::st_bbox(sf::st_as_sf(template)),
      values = NA_real_,
      ny = ny,
      nx = nx
    )

  r <- stars::st_rasterize(data_sf[, c(as.character(var))],
    template = raster_tmplt
  )

  return(r)
}
