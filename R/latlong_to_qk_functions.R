#' Converts lat/long coordinates to pixel XY coordinates
#'
#' @description Converts a point from latitude/longitude WGS-84 coordinates (in degrees) into pixel XY coordinates at a specified level of detail.
#' For further information, refer to the Microsoft Bing Maps Tile System documentation.
#'
#' @param latitude Latitude of the point, in degrees.
#' @param longitude Longitude of the point, in degrees.
#' @param level Level of detail, from 1 (lowest detail) to 23 (highest detail).
#'
#' @return A list returning pixel X and pixel Y coordinates.
#' @export
#'
#' @examples
#'
#' latlong_to_pixelXY(latitude = -35,
#'                  longitude = -50,
#'                  level = 6)
#'
latlong_to_pixelXY <- function(latitude, longitude, level) {

  if (level < 0 | level > 23 | (level %% 1) != 0){
    stop("The level of detail should be an integer between 1 and 23")
  }

  # These values were extracted from Microsoft Bing Maps Tile System documentation

  latitude <- clip(latitude, -85.05112878, 85.05112878)  # Clip latitude within bounds
  longitude <- clip(longitude, -180, 180)  # Clip longitude within bounds

  x <- (longitude + 180) / 360

  sinLatitude <- sin(latitude * pi / 180)

  y <- 0.5 - log((1 + sinLatitude) / (1 - sinLatitude)) / (4 * pi)

  mapsize <- mapsize(level = level)

  pixelX <- as.integer(clip(x * mapsize + 0.5, 0,
                            mapsize - 1))  # Clip and convert to integer
  pixelY <- as.integer(clip(y * mapsize + 0.5, 0,
                            mapsize - 1))  # Clip and convert to integer

  return(list(pixelX = pixelX, pixelY = pixelY))
}



#' Converts pixel XY coordinates into tile XY coordinates
#'
#' @description Converts pixel XY coordinates into tile XY coordinates of the tile containing the specified pixel.
#' For further information, refer to the Microsoft Bing Maps Tile System documentation.
#'
#' @param pixelX Pixel X coordinate.
#' @param pixelY Pixel Y coordinate.
#'
#' @return A list returning the tile X and tile Y coordinates.
#' @export
#'
#' @examples
#'
#' pixelXY_to_tileXY(pixelX = 5916,
#'               pixelY = 9894)
#'
pixelXY_to_tileXY <- function(pixelX, pixelY) {

  tileX <- floor(pixelX / 256)
  tileY <- floor(pixelY / 256)

  return(list(tileX = tileX, tileY = tileY))
}


#' Converts tile XY coordinates into a quadkey.
#'
#' @description Converts tile XY coordinates into a QuadKey at a specified level of detail.
#' For further information, refer to the Microsoft Bing Maps Tile System documentation.
#'
#' @param tileX Tile X coordinate.
#' @param tileY Tile Y coordinate.
#' @param level Level of detail, from 1 (lowest detail) to 23 (highest detail).
#'
#' @return The quadkey number as a string.
#' @export
#'
#' @examples
#'
#' tileXY_to_quadkey(tileX = 23,
#'                   tileY = 38,
#'                   level = 6)
#'
tileXY_to_quadkey <- function(tileX, tileY, level) {

  if (level < 0 | level > 23 | (level %% 1) != 0) {
    stop("The level of detail should be an integer between 1 and 23")
  }

  # get vector with one space as QuadKey's level of detail
  qk <- character(level)

  for (i in level:1) {
    digit <- '0'
    mask <- 2^(i - 1)

    if ((bitwAnd(tileX, mask) != 0)) {
      digit <- as.character(as.numeric(digit) + 1)
    }

    if ((bitwAnd(tileY, mask) != 0)) {
      digit <- as.character(as.numeric(digit) + 2)
    }

    qk[level - i + 1] <- digit
  }

  return(paste(qk, collapse = ''))
}

