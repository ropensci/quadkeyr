#' Convert a QuadKey into tile XY coordinates.
#'
#' @description  For further information, refer to the
#' Microsoft Bing Maps Tile System documentation.
#'
#' @details
#' Converting latitude/longitude coordinates into a QuadKey
#' and then back to latitude/longitude won't yield identical values,
#' unless the initial latitude/longitude coordinates
#' correspond to the upper-left QuadKey's pixel and tile XY coordinates
#' at the same zoom level.
#'
#' Understanding this distinction is crucial for
#' the accurate use of these functions in coordinate conversions.
#'
#' For a detailed explanation on how to use this
#' and other similar `quadkeyr` functions,
#' read the the vignette:
#' \url{https://fernandez-lab-wsu.github.io/quadkeyr/articles/quadkey_to_sf_conversion.html}
#'
#' @references
#' \url{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}
#'
#' @param quadkey A QuadKey as a single string.
#'
#' @return A list returning the tile X, tile Y coordinates and
#' the zoom level.
#' @export
#'
#' @examples
#'
#' quadkey_to_tileXY(quadkey = "213")
#'
quadkey_to_tileXY <- function(quadkey) {
  # The conversion to character is not straightfoward
  # as there could be leading zeros or scientific notation
  if (!is.character(quadkey) || length(quadkey) != 1) {
    stop("Please provide a QuadKey as a single string")
  }
  if (!all(grepl("[0-9]+", quadkey)) |
    any(!unlist(strsplit(quadkey, "")) %in% c("0", "1", "2", "3"))) {
    stop("QuadKeys can contain only the numbers '0', '1', '2', or '3'")
  }

  if (nchar(quadkey) == 0) {
    return(list(
      tileX = 0,
      tileY = 0,
      zoom = 0
    ))
  }

  # Split the digits of the QuadKey and reverse the order
  digits <- rev(strsplit(quadkey, "")[[1]])

  # The number of digits corresponds to the zoom level
  i <- length(digits)

  masks <- 2**(0:(i - 1))
  xt <- sum(masks[digits == "1" | digits == "3"])
  yt <- sum(masks[digits == "2" | digits == "3"])

  return(list(
    tileX = xt,
    tileY = yt,
    zoom = i
  ))
}


#' Convert tile XY coordinates into pixel XY coordinates
#'
#' @description  Converts tile XY coordinates into pixel XY coordinates of
#'  the upper-left pixel of the specified tile.
#' For further information, refer to the Microsoft Bing Maps Tile System
#' documentation.
#'
#' @details
#' Converting latitude/longitude coordinates into a QuadKey
#' and then back to latitude/longitude won't yield identical values,
#' unless the initial latitude/longitude coordinates
#' correspond to the upper-left Quadkey's pixel and tile XY coordinates
#' at the same zoom level.
#'
#' Understanding this distinction is crucial for
#' the accurate use of these functions in coordinate conversions.
#'
#' For a detailed explanation on how to use this
#' and other similar `quadkeyr` functions,
#' read the the vignette:
#' \url{https://fernandez-lab-wsu.github.io/quadkeyr/articles/quadkey_to_sf_conversion.html}
#'
#' @references
#' \url{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}
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


#' Convert pixel XY coordinatess into lat/long coordinates.
#'
#' @description Converts a pixel from pixel XY coordinates at a specified zoom
#' level into latitude/longitude WGS-84 coordinates (in degrees).
#' For further information, refer to the
#' Microsoft Bing Maps Tile System documentation.
#'
#' @details
#' Converting latitude/longitude coordinates into a QuadKey
#' and then back to latitude/longitude won't yield identical values,
#' unless the initial latitude/longitude coordinates
#' correspond to the upper-left QuadKey's pixel and tile XY coordinates
#' at the same zoom level.
#'
#' Understanding this distinction is crucial for
#' the accurate use of these functions in coordinate conversions.
#'
#' For a detailed explanation on how to use this
#' and other similar `quadkeyr` functions,
#' read the the vignette:
#' \url{https://fernandez-lab-wsu.github.io/quadkeyr/articles/quadkey_to_sf_conversion.html}
#'
#' @references
#' \url{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}
#'
#' @param pixelX X coordinate of the point, in pixels.
#' @param pixelY Y coordinates of the point, in pixels.
#' @param zoom Zoom or level of detail,
#' from 1 (lowest detail) to 23 (highest detail).
#'
#' @return A list with the longitude and latitude.
#' @export
#'
#' @examples
#'
#' pixelXY_to_latlong(
#'   pixelX = 768,
#'   pixelY = 1280,
#'   zoom = 3
#' )
#'
pixelXY_to_latlong <- function(pixelX, pixelY, zoom) {
  if (zoom < 0 | zoom > 23 | (zoom %% 1) != 0) {
    stop("The zoom level should be an integer between 1 and 23")
  }

  # Check if pixelX and pixelY are within the valid range
  max_pixel_value <- mapsize(zoom) 
  if (pixelX < 0 | pixelX > max_pixel_value |
    pixelY < 0 | pixelY > max_pixel_value) {
    stop(paste(
      "Invalid pixelX or pixelY values.",
      "They should be within the range [0, (256*2^zoom) - 1]."
    ))
  }

  mapsize <- mapsize(zoom)

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
#' @details
#' Converting latitude/longitude coordinates into a QuadKey
#' and then back to latitude/longitude won't yield identical values,
#' unless the initial latitude/longitude coordinates
#' correspond to the upper-left Quadkey's pixel and tile XY coordinates
#' at the same zoom level.
#'
#' Understanding this distinction is crucial for
#' the accurate use of these functions in coordinate conversions.
#'
#' For a detailed explanation on how to use this
#' and other similar `quadkeyr` functions,
#' read the the vignette:
#' \url{https://fernandez-lab-wsu.github.io/quadkeyr/articles/quadkey_to_sf_conversion.html}
#'
#' @param quadkey_data A single QuadKey as a string or
#' a vector with unique QuadKeys.
#'
#' @seealso \code{\link{quadkey_to_tileXY}}
#' @seealso \code{\link{tileXY_to_pixelXY}}
#' @seealso \code{\link{pixelXY_to_latlong}}
#' @references
#' \url{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}
#'
#' @return A sf POINT data.frame with a `quadkey` column.
#' The latitude/longitude coordinates represent
#' the upper-left corner of the QuadKey.
#'
#' @export
#'
#' @examples
#'
#' quadkey_to_latlong(quadkey_data = "213")
#' quadkey_to_latlong(quadkey_data = c("213", "212", "210"))
#'
quadkey_to_latlong <- function(quadkey_data) {
  # The conversion to character is not straightfoward
  # as there could be leading zeros or scientific notation
  if (!is.character(quadkey_data)) {
    stop("Please provide QuadKeys a single string or a character vector")
  }
  if (any(duplicated(quadkey_data))) {
    stop("Please, remove duplicated QuadKeys")
  }
  if (any(unique(nchar(quadkey_data)) != nchar(quadkey_data[1]))) {
    stop("All the QuadKeys should have the same number of digits")
  }
  if (!all(grepl("[0-9]+", quadkey_data)) |
    any(!unlist(strsplit(quadkey_data, "")) %in% c("0", "1", "2", "3"))) {
    stop("QuadKeys can contain only the numbers '0', '1', '2', or '3'")
  }

  zoom <- quadkey_to_tileXY(quadkey_data[1])$zoom

  datacoords <- c()
  data <- data.frame(quadkey_data = NA)

  for (i in seq_along(quadkey_data)) {
    data[i, "quadkey"] <- quadkey_data[i]

    data[i, c("tileX", "tileY", "zoom")] <- quadkey_to_tileXY(quadkey_data[i])

    data[i, c("pixelX", "pixelY")] <- tileXY_to_pixelXY(
      data$tileX[i],
      data$tileY[i]
    )

    data[i, c("lat", "lon")] <- pixelXY_to_latlong(
      data$pixelX[i],
      data$pixelY[i],
      zoom <- unique(data$zoom)
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
