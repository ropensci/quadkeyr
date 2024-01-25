#' Map size in pixels
#'
#' @description  Determines the map width and height (in pixels) at a specified
#' level of detail.
#' For further information, refer to the Microsoft Bing Maps Tile 
#' System documentation.
#'
#' @param level Level of detail, from 1 (lowest detail) to 23 (highest detail).
#'
#' @return The map width and height in pixels.
#' @export
#'
#' @examples
#'
#' mapsize(level = 6)

# At the lowest level of detail (level 1), the map is 512 x 512 pixels.
# At each successive level of detail, the map width and height grow by a
# factor of 2.
# The pixel at the lower right corner of the map has pixel coordinates
# (width-1, height-1) or ((256*2^level)–1, (256 * 2^level)–1).
# Please, read the official documentation for more details.
mapsize <- function(level) {
  return(256 * 2^level)
}


#' Clips a number to the specified minimum and maximum values.
#'
#' @description This function is user internally by latlong_to_pixelXY and 
#' pixelXY_to_latlong.
#' For further information, refer to the Microsoft Bing Maps Tile System 
#' documentation.
#'
#' @param n The number to clip.
#' @param min_value Minimum allowable value.
#' @param max_value Maximum allowable value.
#'
#' @return The clipped value.
#' @keywords internal
#'
#' @examples
#'
#'      clip(n = 1,
#'      min_value = 3,
#'      max_value = 5)
#'

clip <- function(n, min_value, max_value) {

  return(min(max(n, min_value), max_value))
}

#' Ground resolution at a specified latitude and level of detail
#'
#' @description Determines the ground resolution (in meters per pixel) at a specified latitude and level of detail.
#' For further information, refer to the Microsoft Bing Maps Tile System documentation.
#'
#' @param latitude Latitude (in degrees) at which to measure the ground resolution.
#' @param level Level of detail, from 1 (lowest detail) to 23 (highest detail).
#'
#' @return the ground resolution (meters / pixel)
#' @export
#'
#' @examples
#'
#' ground_res(latitude = 0,
#'            level = 6)
#'

ground_res <- function(latitude, level) {

  # Values in Microsoft Bing Tile System Documentation
  min_latitude <- -85.05112878
  max_latitude <- 85.05112878
  min_longitude <- -180
  max_longitude <- 180
  earth_radius <- 6378137

  latitude <- clip(latitude, min_latitude, max_latitude)
  mapsize <- mapsize(level)

  return(cos(latitude * pi / 180) * 2 * pi * earth_radius / mapsize)
}

#' Map scale (1 : N)
#'
#' @description Determines the map scale at a specified latitude, 
#' level of detail, and screen resolution.
#' For further information, refer to the Microsoft Bing Maps Tile System 
#' documentation.
#'
#' @param latitude Latitude (in degrees) at which to measure the map scale.
#' @param level Level of detail, from 1 (lowest detail) to 23 (highest detail).
#' @param screen_dpi Resolution of the screen, in dots per inch.
#'
#' @return The map scale, expressed as the denominator N of the ratio 1 : N.
#' @export
#'
#' @examples
#'
#' mapscale(latitude = 0,
#'          level = 6,
#'          screen_dpi = 96)
#'

# Values of constants extracted from Microsoft Bing Tile System Documentation
mapscale <- function(latitude, level, screen_dpi) {
  return(ground_res(latitude, level) * screen_dpi / 0.0254)
}

