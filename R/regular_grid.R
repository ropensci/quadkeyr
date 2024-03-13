#' Convert a incomplete QuadKey `sf` POINT data.frame into a regular grid.
#'
#' @description This function completes `sf` POINT data.frame grid of QuadKeys
#' using the bounding box of the data provided.
#'
#' @param data A `sf` POINT data.frame
#'
#' @seealso \code{\link{create_qk_grid}}
#' @seealso \code{\link{quadkey_to_latlong}}
#'
#' @return A list with three elements:
#' * `data` A `sf` POINT data.frame, with the rows needed to complete the grid.
#' * `num_rows` The number of columns of the regular grid.
#' * `num_cols` The number of rows of the regular grid.
#'
#' @export
#' @examples
#'
#' quadkey_vector <- c("213", "210", "211")
#'
#' qtll <- quadkey_to_latlong(quadkey = quadkey_vector)
#'
#' regular_qk_grid(qtll)
regular_qk_grid <- function(data) {
  # I need the data.frame to be class simple features to can
  # estimate the bounding box
  bbox <- sf::st_bbox(data)

  # I assume that the all the QuadKeys correspond to the same zoom level.
  qk_zoom <- nchar(data$quadkey[1])

  # Create the complete grid
  grid <- create_qk_grid(
    xmin = bbox$xmin[[1]],
    xmax = bbox$xmax[[1]],
    ymin = bbox$ymin[[1]],
    ymax = bbox$ymax[[1]],
    zoom = qk_zoom
  )

  if (nrow(grid$data) == nrow(data)) {
    warning("The grid is already complete, this function is not necessary")
    return(list(
      data = data,
      num_rows = grid$num_rows,
      num_cols = grid$num_cols
    ))
  }

  # Select the QuadKeys that are missing in the original grid
  qk_missing <- grid$data |>
    dplyr::anti_join(data, by = "quadkey") |> # tidyselect
    dplyr::select(-"tileX", -"tileY") # tidyselect

  # Convert the QuadKeys to coordinates
  grid_coords <- quadkey_to_latlong(qk_missing$quadkey)


  if (ncol(grid_coords) == ncol(data)) {
    # Add the missing QuadKey coordinates to the original dataset
    data <- rbind(grid_coords, data)
  } else {
    # this function will introduce NAs if there are columns
    # different than quadkey and geometry
    # probably used inside add_regular_polygon_grid()
    data <- dplyr::bind_rows(grid_coords, data)
  }

  return(list(
    data = data,
    num_rows = grid$num_rows,
    num_cols = grid$num_cols
  ))
}

#' Add the rows needed to complete a regular QuadKey polygon grid
#' derived from the bounding box of the `quadkey` column of a data.frame.
#'
#' @description
#' This function estimates the bounding box of the quadkeys given in the
#' quadkey column and adds rows to complete the quadkeys and the geometry
#' needed to create a regular grid.
#' All other columns for the introduced QuadKeys will be filled with NAs.
#'
#' For a detailed explanation on how to use this
#' and other similar `quadkeyr` functions,
#' read the the vignette:
#' \url{https://fernandez-lab-wsu.github.io/quadkeyr/articles/quadkey_identified_data_to_raster.html}
#'
#' @param data A data.frame with a `quadkey` column.
#'
#' @return A list with three elements:
#' * `data` A `sf` POLYGON data.frame with all the QuadKeys within
#' the bounding box of the ones provided in the `quadkey` column
#' of the input dataset, and the rest of the original variables.
#' The columns `quadkey` and `geometry` are returned for all the grid,
#' The values of the newly added QuadKeys will be NA
#' for the rest of the variables.
#' * `num_rows` The number of columns of the regular grid.
#' * `num_cols` The number of rows of the regular grid.
#'
#' @export
#' @examples
#' # read the file with the data
#' path <- paste0(
#'   system.file("extdata", package = "quadkeyr"),
#'   "/cityA_2020_04_15_0000.csv"
#' )
#' data <- read.csv(path)
#' data <- format_fb_data(data)
#'
#' add_regular_polygon_grid(data = data)
add_regular_polygon_grid <- function(data) {
  sf_grid <- get_qk_coord(data)
  reggrid <- regular_qk_grid(sf_grid)
  add_polygrid <- grid_to_polygon(reggrid$data)
  return(list(
    data = add_polygrid,
    num_cols = reggrid$num_cols,
    num_rows = reggrid$num_rows
  ))
}

#' Get regular QuadKey polygon grid derived from
#' the bounding box of the `quadkey` column of a data.frame.
#'
#' @description
#' This function estimates the bounding box of the QuadKeys given in the
#' `quadkey` column and adds the rows needed to complete a regular grid.
#'
#' For a detailed explanation on how to use this
#' and other similar `quadkeyr` functions,
#' read the the vignette:
#' \url{https://fernandez-lab-wsu.github.io/quadkeyr/articles/facebook_mobility_csvs_to_raster_files.html}
#'
#' @param data A data.frame with a `quadkey` column.
#'
#' @return A list with three elements:
#' * `data` An `sf` POLYGON data.frame with all the QuadKeys within
#' the bounding box of the `quadkey` column of a data.frame.
#' Only the columns `quadkey`, `tileX`, `tileY` and `geometry` are returned.
#' * `num_rows` The number of columns of the regular grid.
#' * `num_cols` The number of rows of the regular grid.
#'
#' @export
#' @examples
#'
#' # data file
#' path <- paste0(
#'   system.file("extdata", package = "quadkeyr"),
#'   "/cityA_2020_04_15_0000.csv"
#' )
#' data <- read.csv(path)
#' data <- format_fb_data(data)
#'
#' get_regular_polygon_grid(data = data)
get_regular_polygon_grid <- function(data) {

  # I convert the QuadKeys to points and not polygons directly
  # Because I want to generate the regular grid first
  grid_coords <- quadkey_to_latlong(quadkey_data = unique(data$quadkey))
  reggrid <- regular_qk_grid(grid_coords)
  polygrid <- grid_to_polygon(reggrid$data)
  return(list(
    data = polygrid,
    num_cols = reggrid$num_cols,
    num_rows = reggrid$num_rows
  ))
}
