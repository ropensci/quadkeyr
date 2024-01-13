#' Extract lat/long coordinates from the QuadKey number
#'
#' @description Reads the QuadKey number as a string to extract the
#' lat/long coordinates of the upper-left corner of the QuadKey.
#'
#' @param data A dataframe with a column named quadkey
#'
#' @seealso \code{\link{quadkey_to_tileXY}}
#' @seealso \code{\link{tileXY_to_pixelXY}}
#' @seealso \code{\link{pixelXY_to_latlong}}
#'
#' @return A spatial dataframe (sf) containing the tiles XY coordinates
#' (tileX, tileY), the QuadKey number (quadkey), and a column for POINT
#' geometry.
#'
#' @export
#'
#' @examples
#'
#' grid <- create_qk_grid(xmin = -59,
#'                        xmax = -40,
#'                        ymin = -38,
#'                        ymax = -20,
#'                        level = 6)
#'
#' grid_coords <- extract_qk_coord(data = grid$data)
#'
#' plot(grid_coords)
extract_qk_coord <- function(data){

  if (!"quadkey" %in% colnames(data)) {
    stop("Please ensure that the dataset contains a column named 'quadkey'.")
  }

  for(i in seq_len(nrow(data))){
    # check that the data has the correct dimensions for this analysis
    level  <-  nchar(data$quadkey[i])

    qktot  <-  quadkey_to_tileXY(data$quadkey[i])

    data$qk_tileX[i] <- qktot$tileX
    data$qk_tileY[i] <- qktot$tileY

    ttop <- tileXY_to_pixelXY(tileX = data$qk_tileX[i],
                              tileY = data$qk_tileY[i])

    data$tl_pxx[i] <- ttop$pixelX
    data$tl_pxy[i] <- ttop$pixelY

    ptoll <- pixelXY_to_latlong(pixelX = data$tl_pxx[i],
                               pixelY = data$tl_pxy[i],
                               level = level)

    data$pxy_lat[i] <- ptoll$lat
    data$pxy_lon[i] <- ptoll$lon

  }

  data <- data |>
    dplyr::select("tileX", "tileY", "quadkey",
                  "pxy_lon", "pxy_lat") |> # tidyselect
    sf::st_as_sf(coords = c("pxy_lon", "pxy_lat"), crs = 4326)

  return(data)

}


#' Extract lat/long coordinates from the tile XY coordinates.
#'
#' @description Reads the tile XY coordinates and extracts the
#' lat/long coordinates of the upper-left corner of the QuadKey.
#'
#' @param data A dataframe with columns named tileX and tileY
#' @param level Level of detail, from 1 (lowest detail) to 23 (highest detail).
#'
#' @seealso \code{\link{tileXY_to_pixelXY}}
#' @seealso \code{\link{pixelXY_to_latlong}}
#'
#' @return  A spatial dataframe (sf) containing the tiles XY coordinates
#' (tileX, tileY)  and a column for POINT geometry.
#'
#' @export
#'
#' @examples
#'
#'
#' grid <- create_qk_grid(xmin = -59,
#'                        xmax = -40,
#'                        ymin = -38,
#'                        ymax = -20,
#'                        level = 6)
#'
#' extract_tile_coord(data = grid$data,
#'                    level = 6)
#'
extract_tile_coord <- function(data, level){

  if (!any(c('tileX', 'tileY') %in% colnames(data))) {
    stop(paste("Please ensure that the dataset contains columns named 'tileX'",
               "and 'tileY'"))
  }

  for(i in seq_len(nrow(data))){
   # conversion from tile coordinates to geographic coordinates
    ttop <- tileXY_to_pixelXY(tileX = data$tileX[i],
                             tileY = data$tileY[i])

    data$tl_pxx[i] <- ttop$pixelX
    data$tl_pxy[i] <- ttop$pixelY

    ptoll <- pixelXY_to_latlong(pixelX = data$tl_pxx[i],
                               pixelY = data$tl_pxy[i],
                               level = level)
 
    data$pxy_lat[i] <- ptoll$lat
    data$pxy_lon[i] <- ptoll$lon

  }

  # I have to keep the quadkeys for later use
  data <- data |>
    dplyr::select("tileX", "tileY", "quadkey",
                  "pxy_lon", "pxy_lat") |> # tidyselect
    sf::st_as_sf(coords = c('pxy_lon', 'pxy_lat'), crs = 4326)

  return(data)

}

