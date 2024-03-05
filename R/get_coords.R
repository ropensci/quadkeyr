#' Get lat/long coordinates from the QuadKey
#'
#' @description Reads the QuadKey as a string and extracts the
#' lat/long coordinates of the upper-left corner of the QuadKey.
#'
#' @param data A dataframe with a quadkey column.
#'
#' @seealso \code{\link{quadkey_to_tileXY}}
#' @seealso \code{\link{tileXY_to_pixelXY}}
#' @seealso \code{\link{pixelXY_to_latlong}}
#'
#' @return A `sf` POINT data.frame containing the tiles XY coordinates
#' (`tileX`, `tileY`), the QuadKeys (`quadkey`), and a `geometry` column.
#'
#' @export
#'
#' @examples
#'
#' grid <- create_qk_grid(
#'   xmin = -59,
#'   xmax = -40,
#'   ymin = -38,
#'   ymax = -20,
#'   zoom = 6
#' )
#'
#' # quadkey column in grid$data converted to geographic coordinates
#' grid_coords <- get_qk_coord(data = grid$data)
#'
#' plot(grid_coords)
get_qk_coord <- function(data) {
  if (!"quadkey" %in% colnames(data)) {
    stop("Please ensure that the dataset contains a column named 'quadkey'.")
  }

  # initialize columns to avoid warnings
  data$tileX <- NA
  data$tileY <- NA

  for (i in seq_len(nrow(data))) {
    # check that the data has the correct dimensions for this analysis
    zoom <- nchar(data$quadkey[i])

    qktot <- quadkey_to_tileXY(data$quadkey[i])

    data$tileX[i] <- qktot$tileX
    data$tileY[i] <- qktot$tileY
  }

  data <- get_tile_coord(
    data = data,
    zoom = zoom
  )

  return(data)
}


#' Get lat/long coordinates from the tile XY coordinates.
#'
#' @description Reads the tile XY coordinates and extracts the
#' lat/long coordinates of the upper-left corner of the QuadKey.
#'
#' @param data A dataframe with columns named `tileX` and `tileY`
#' @param zoom Zoom or Level of detail,
#' from 1 (lowest detail) to 23 (highest detail).
#'
#' @seealso \code{\link{tileXY_to_pixelXY}}
#' @seealso \code{\link{pixelXY_to_latlong}}
#'
#' @return  A `sf` POINT data.frame containing the tiles XY coordinates
#' (`tileX`, `tileY`), a `quadkey` and a `geometry` column.
#'
#' @keywords internal
#' @examples
#' grid <- create_qk_grid(
#'   xmin = -59,
#'   xmax = -40,
#'   ymin = -38,
#'   ymax = -20,
#'   zoom = 6
#' )
#'
#' # tileX and tileY columns in grid$data converted to geographic coordinates
#' get_tile_coord(
#'   data = grid$data,
#'   zoom = 6
#' )
#'
get_tile_coord <- function(data, zoom) {
  if (!any(c("tileX", "tileY") %in% colnames(data))) {
    stop(paste(
      "Please ensure that the dataset contains columns named 'tileX'",
      "and 'tileY'"
    ))
  }


  # initialize columns to avoid warnings
  data$pixelX <- NA
  data$pixelY <- NA
  data$lat <- NA
  data$lon <- NA

  for (i in seq_len(nrow(data))) {
    # conversion from tile coordinates to geographic coordinates
    ttop <- tileXY_to_pixelXY(
      tileX = data$tileX[i],
      tileY = data$tileY[i]
    )

    data$pixelX[i] <- ttop$pixelX
    data$pixelY[i] <- ttop$pixelY

    ptoll <- pixelXY_to_latlong(
      pixelX = data$pixelX[i],
      pixelY = data$pixelY[i],
      zoom = zoom
    )

    data$lat[i] <- ptoll$lat
    data$lon[i] <- ptoll$lon
  }

  # I have to keep the quadkeys for later use
  data <- data |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    dplyr::select(
      -"pixelX", -"pixelY",
    ) # tidyselect

  return(data)
}
