#' Converts lat/long coordinates to pixel XY coordinates
#'
#' @description Converts a point from latitude/longitude WGS-84 coordinates
#' (in degrees) into pixel XY coordinates at a specified zoom level.
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
#' @param lat Latitude of the point, in degrees.
#' @param lon Longitude of the point, in degrees.
#' @param zoom Zoom or level of detail,
#' from 1 (lowest detail) to 23 (highest detail).
#'
#' @return A list returning pixel X and pixel Y coordinates.
#' @export
#'
#' @examples
#'
#' latlong_to_pixelXY(
#'   lat = -35,
#'   lon = -50,
#'   zoom = 6
#' )
#'
latlong_to_pixelXY <- function(lat, lon, zoom) {
  if (zoom < 0 | zoom > 23 | (zoom %% 1) != 0) {
    stop("The zoom level should be an integer between 1 and 23")
  }
  
  # These values were extracted from Microsoft Bing Maps Tile System
  # documentation
  
  latitude <- clip(lat,-85.05112878, 85.05112878) # Clip latitude
  longitude <- clip(lon,-180, 180) # Clip longitude
  
  x <- (longitude + 180) / 360
  
  sinLatitude <- sin(latitude * pi / 180)
  
  y <- 0.5 - log((1 + sinLatitude) / (1 - sinLatitude)) / (4 * pi)
  
  mapsize <- mapsize(zoom = zoom)
  
  pixelX <- as.integer(clip(x * mapsize + 0.5, 0,
                            mapsize - 1)) # Clip and convert to integer
  pixelY <- as.integer(clip(y * mapsize + 0.5, 0,
                            mapsize - 1)) # Clip and convert to integer
  
  return(list(pixelX = pixelX, pixelY = pixelY))
}

#' Converts pixel XY coordinates into tile XY coordinates
#'
#' @description Converts pixel XY coordinates into tile XY coordinates of the
#' tile containing the specified pixel.
#' For further information, refer to the Microsoft Bing Maps
#' Tile System documentation.
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
#' @param pixelX Pixel X coordinate.
#' @param pixelY Pixel Y coordinate.
#'
#' @return A list returning the tile X and tile Y coordinates.
#' @export
#'
#' @examples
#'
#' pixelXY_to_tileXY(
#'   pixelX = 5916,
#'   pixelY = 9894
#' )
#'
pixelXY_to_tileXY <- function(pixelX, pixelY) {
  # Each tile is 256x256 pixels.
  # These functions are described in the Microsoft Bing Map Tile System
  # documentation.
  
  tileX <- floor(pixelX / 256)
  tileY <- floor(pixelY / 256)
  
  return(list(tileX = tileX, tileY = tileY))
}


#' Converts tile XY coordinates into a QuadKey.
#'
#' @description Converts tile XY coordinates into a QuadKey at a specified
#' zoom level.
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
#' @param zoom Zoom or level of detail, 
#' from 1 (lowest detail) to 23 (highest detail).
#'
#' @return The QuadKey as a string.
#' @export
#'
#' @examples
#'
#' tileXY_to_quadkey(
#'   tileX = 23,
#'   tileY = 38,
#'   zoom = 6
#' )
#'
tileXY_to_quadkey <- function(tileX, tileY, zoom) {
  # Give an error if the zoom level isn't between 0 and 23
  # or it is not an integer.
  if (zoom < 0 | zoom > 23 | (zoom %% 1) != 0) {
    stop("The zoom level should be an integer between 1 and 23")
  }
  
  # Check if tileX and tileY are within the valid range
  max_tile_value <- (2 ^ zoom) - 1
  if (tileX < 0 |
      tileX > max_tile_value | tileY < 0 | tileY > max_tile_value) {
    stop(
      paste(
        "Invalid tileX or tileY values.",
        "They should be within the range [0, 2^zoom - 1]."
      )
    )
  }
  
  # get vector with one space as QuadKey's zoom level.
  qk <- character(zoom)
  
  for (i in zoom:1) {
    digit <- "0"
    mask <- 2 ^ (i - 1)
    
    if ((bitwAnd(tileX, mask) != 0)) {
      digit <- as.character(as.numeric(digit) + 1)
    }
    
    if ((bitwAnd(tileY, mask) != 0)) {
      digit <- as.character(as.numeric(digit) + 2)
    }
    
    qk[zoom - i + 1] <- digit
  }
  
  return(paste(qk, collapse = ""))
}

#' Convert latitude/longitude coordinates into QuadKeys
#'
#' @description Converts a point from latitude/longitude WGS-84 coordinates
#' (in degrees) into a Quadkey at a specified zoom level.
#' For further information, refer to the Microsoft Bing Maps Tile
#' System documentation.
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
#' @param lat Latitude of the point, in degrees.
#' @param lon Longitude of the point, in degrees.
#' @param zoom Zoom or level of detail, from 1 (lowest detail) 
#' to 23 (highest detail).
#'
#' @return A dataframe with latitude (lat), longitude (lon), zoom level
#' (zoom) and the QuadKey as a string (quadkey).
#' @export
#'
#' @examples
#'
#' latlong_to_quadkey(
#'   lat = 35.63051,
#'   lon = 139.69116,
#'   zoom = 20
#' )
#' latlong_to_quadkey(lat = c(-4, -25.33, -25.66),
#'                    lon = c(-53, -60.33, -70.66),
#'                    zoom = 4)
latlong_to_quadkey <- function(lat, lon, zoom) {
  # Give an error if the zoom level isn't between 0 and 23
  # or it is not an integer.
  if (zoom < 0 | zoom > 23 | (zoom %% 1) != 0) {
    stop("The zoom level should be an integer between 1 and 23")
  }
  
  
  data <- data.frame(lat = lat,
                     lon = lon,
                     zoom = zoom)
  
  
  for (i in seq_len(nrow(data))) {
    data[i, c("pixelX", "pixelY")] <- latlong_to_pixelXY(data$lat[i],
                                                         data$lon[i],
                                                         data$zoom[i])
    
    data[i, c("tileX", "tileY")] <- pixelXY_to_tileXY(data$pixelX[i],
                                                      data$pixelY[i])
    
    data[i, "quadkey"] <- tileXY_to_quadkey(data$tileX[i],
                                            data$tileY[i],
                                            zoom = data$zoom[i])
  }
  
  data_sf <- data |>
    dplyr::select("lat", "lon", "quadkey") |> # tidyselect
    sf::st_as_sf(coords = c("lon", "lat"),
                 crs = 4326)
  
  return(data_sf)
}
