#' Converts a QuadKey into tile XY coordinates.
#'
#' @description  For further information, refer to the
#' Microsoft Bing Maps Tile System documentation.
#'
#' @seealso
#' \href{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}{
#' Microsoft Bing Maps Tile System documentation}
#'
#' @param qk The QuadKey number as a string.
#'
#' @return A list returning the tile X, tile Y coordinates and
#' the level of detail.
#' @export
#'
#' @examples
#'
#' quadkey_to_tileXY(qk = 213)
#'
quadkey_to_tileXY <- function(qk) {
  qk <- as.character(qk)

  if (nchar(qk) == 0) {
    return(list(
      tileX = 0,
      tileY = 0,
      level = 0
    ))
  }

  # Split the digits of the QuadKey and reverse the order
  digits <- rev(strsplit(qk, "")[[1]])

  # The number of digits corresponds to the zoom level
  i <- length(digits)

  masks <- 2**(0:(i - 1))
  xt <- sum(masks[digits == "1" | digits == "3"])
  yt <- sum(masks[digits == "2" | digits == "3"])

  return(list(
    tileX = xt,
    tileY = yt,
    level = i
  ))
}


#' Convert tile XY coordinates into pixel XY coordinates
#'
#' @description  Converts tile XY coordinates into pixel XY coordinates of
#'  the upper-left pixel of the specified tile.
#' For further information, refer to the Microsoft Bing Maps Tile System
#' documentation.
#'
#' @seealso
#' \href{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}{
#' Microsoft Bing Maps Tile System documentation}
#'
#' @param tileX Tile X coordinate.
#' @param tileY Tile Y coordinate.
#'
#' @return A list returning the pixel X and pixel Y coordinates.
#' @export
#'
#' @examples
#'
#' tileXY_to_pixelXY(
#'   tileX = 3,
#'   tileY = 5
#' )
#'
tileXY_to_pixelXY <- function(tileX, tileY) {

  # Each tile is 256x256 pixels.
  # These functions are described in the Microsoft Bing Map Tile System
  # documentation.

  pixelX <- tileX * 256
  pixelY <- tileY * 256

  return(list(
    pixelX = pixelX,
    pixelY = pixelY
  ))
}


#' Converts pixel XY coordinatess into lat/long coordinates.
#'
#' @description Converts a pixel from pixel XY coordinates at a specified level
#' of detail into latitude/longitude WGS-84 coordinates (in degrees).
#' For further information, refer to the
#' Microsoft Bing Maps Tile System documentation.
#'
#' @seealso
#' \href{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}{
#' Microsoft Bing Maps Tile System documentation}
#'
#' @param pixelX X coordinate of the point, in pixels.
#' @param pixelY Y coordinates of the point, in pixels.
#' @param level Level of detail, from 1 (lowest detail) to 23 (highest detail).
#'
#' @return A list with the longitude and latitude.
#' @export
#'
#' @examples
#'
#' pixelXY_to_latlong(
#'   pixelX = 768,
#'   pixelY = 1280,
#'   level = 3
#' )
#'
pixelXY_to_latlong <- function(pixelX, pixelY, level) {
  if (level < 0 | level > 23 | (level %% 1) != 0) {
    stop("The level of detail should be an integer between 1 and 23")
  }

  # Check if pixelX and pixelY are within the valid range
  max_pixel_value <- mapsize(level) - 1
  if (pixelX < 0 | pixelX > max_pixel_value | pixelY < 0 | pixelY > max_pixel_value) {
    stop(paste(
      "Invalid pixelX or pixelY values.",
      "They should be within the range [0, (256*2^level) - 1]."
    ))
  }

  mapsize <- mapsize(level)

  x <- (clip(
    pixelX, 0,
    mapsize - 1
  ) / mapsize) - 0.5

  y <- 0.5 - (clip(
    pixelY, 0,
    mapsize - 1
  ) / mapsize)

  latitude <- 90 - 360 * atan(exp(-y * 2 * pi)) / pi
  longitude <- 360 * x

  return(list(
    lat = latitude,
    lon = longitude
  ))
}


#' Convert a string of Quadkey numbers to lat/long coordinates
#'
#' @description This function converts Quadkeys to
#' latitude/longitude WGS-84 coordinates (in degrees).
#' For further information, refer to the Microsoft Bing Maps Tile System
#' documentation.
#'
#' @param quadkeys A vector with unique QuadKey numbers.
#'
#' @seealso \code{\link{quadkey_to_tileXY}}
#' @seealso \code{\link{tileXY_to_pixelXY}}
#' @seealso \code{\link{pixelXY_to_latlong}}
#' @seealso
#' \href{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}{
#' Microsoft Bing Maps Tile System documentation}
#'
#' @return A spatial data frame of class sf with a quadkey column
#' and POINT geometry.
#' The coordinates represent the upper-left corner of the QuadKey.
#'
#' @export
#'
#' @examples
#'
#' quadkey_to_latlong(c("213", "212", "210"))
#'
quadkey_to_latlong <- function(quadkeys) {
  if (any(duplicated(quadkeys))) {
    stop("Please, remove duplicated QuadKeys")
  }


  if (any(unique(nchar(quadkeys)) != nchar(quadkeys[1]))) {
    stop("All the QuadKeys should have the same number of digits")
  }

  level <- quadkey_to_tileXY(quadkeys[1])$level

  datacoords <- c()
  data <- data.frame(quadkey = NA)

  for (i in seq_along(quadkeys)) {
    data[i, "quadkey"] <- quadkeys[i]

    data[i, c("tileX", "tileY", "level")] <- quadkey_to_tileXY(quadkeys[i])

    data[i, c("pixelX", "pixelY")] <- tileXY_to_pixelXY(
      data$tileX[i],
      data$tileY[i]
    )

    data[i, c("lat", "lon")] <- pixelXY_to_latlong(data$pixelX[i],
      data$pixelY[i],
      level = unique(data$level)
    )

    datacoords <- rbind(data[i, ], datacoords)
  }

  data_sf <- datacoords |>
    dplyr::select("quadkey", "lat", "lon") |> # tidyselect
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    )

  return(data_sf)
}
