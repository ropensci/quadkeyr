#' Converts a irregular QuadKey grid into a regular grid
#'
#' @description This function completes the grid of QuadKeys using the bounding
#' box of the data provided.
#'
#' @param data A spatial dataset of class sf with POINT geometry
#'
#' @seealso \code{\link{create_qk_grid}}
#' @seealso \code{\link{quadkey_to_latlong}}
#'
#' @return A spatial dataset of class sf with POINT geometry
#' @export
#'
#' @examples
#'
#' quadkeys <- c("213", "210", "211")
#'
#' qtll <- quadkey_to_latlong(quadkeys = quadkeys)
#'
#' regular_qk_grid(qtll)
#'
regular_qk_grid <- function(data) {
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
    return(data)
  }

  # Select the QuadKeys that are missing in the original grid
  qk_missing <- grid$data |>
    dplyr::anti_join(data, by = "quadkey")
  
  # Convert the QuadKeys to coordinates
  grid_coords <- quadkey_to_latlong(qk_missing$quadkey)

  # Add the missing QuadKey coordinates to the original dataset
  data <- rbind(grid_coords, data)

  # I will need the tiles for grid_to_polygon
  for (i in seq_len(nrow(data))) {
    data[i, "tileX"] <- quadkey_to_tileXY(data$quadkey[i])$tileX
    data[i, "tileY"] <- quadkey_to_tileXY(data$quadkey[i])$tileY
  }

  return(list(
    data = data,
    num_rows = grid$num_rows,
    num_cols = grid$num_cols
  ))
}
