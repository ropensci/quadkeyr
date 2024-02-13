#' Create grid of QuadKeys for a particular zoom or level of detail.
#'
#' @description Generates a grid comprising all the QuadKeys within the area
#' defined by the maximum and minimum coordinates of latitude and
#' longitude along with a specified zoom level.
#'
#'
#' @param xmin Minimum value in the x axis (longitude)
#' @param xmax Maximum value in the y axis (latitude)
#' @param ymin Minimum value in the x axis (longitude)
#' @param ymax Maximum value in the Y axis (latitude)
#' @param zoom Zoom or level of detail, 
#' from 1 (lowest detail) to 23 (highest detail).
#'
#' @importFrom rlang .data
#'
#' @return A list returning the QuadKeys as a dataframe (data),
#' the number of rows (num_rows)
#' and columns (num_cols) of the grid.
#'
#' @export
#'
#' @examples
#'
#' grid <- create_qk_grid(
#'   xmin = -59,
#'   xmax = -57,
#'   ymin = -35,
#'   ymax = -34,
#'   zoom = 12
#' )
#'
create_qk_grid <- function(xmin, xmax, ymin, ymax, zoom) {

  # Values in Microsoft Bing Tile System Documentation
  min_latitude <- -85.05112878
  max_latitude <- 85.05112878
  min_longitude <- -180
  max_longitude <- 180

  if (zoom < 0 | zoom > 23 | (zoom %% 1) != 0) {
    stop("The zoom level should be an integer between 1 and 23")
  }

  # this variables were defined in the function ground_res
  if (ymin < min_latitude || ymax > max_latitude ||
    xmin < min_longitude || xmax > max_longitude) {
    stop(paste(
      "At least one of the provided coordinates are outside",
      "the valid range. Latitude must be between -85.05112878",
      "and 85.05112878. Longitude must be between -180 and 180."
    ))
  }

  # x - Convert lat/long coordinates to tile XY coords
  pixs <- latlong_to_pixelXY(
    lat = ymin,
    lon = xmin,
    zoom = zoom
  )

  tilesmn <- pixelXY_to_tileXY(
    pixelX = pixs$pixelX,
    pixelY = pixs$pixelY
  )

  # y - Convert lat/long coordinates to tile XY coords
  pixs <- latlong_to_pixelXY(
    lat = ymax,
    lon = xmax,
    zoom = zoom
  )

  tilesmx <- pixelXY_to_tileXY(
    pixelX = pixs$pixelX,
    pixelY = pixs$pixelY
  )

  # How many tile XY coordinates conform the grid?
  resy <- tilesmx$tileY - tilesmn$tileY
  resx <- tilesmx$tileX - tilesmn$tileX

  if (resx == 0 | resy == 0) {
    stop(paste(
      "The selected inputs fail to generate a grid due to the limited area",
      "for this zoom level. Consider adjusting the zoom level",
      "or modifying the xmin, xmax, ymin, or ymax values."
    ))
  }

  # define the dimensions of the matrix
  num_tiles_rows <- abs(resy) 
  num_tiles_cols <- abs(resx) 

  # create all the possible combinations of columns and rows
  # I start with 0 because I want the first tiles in `tilesmn$tileX`
  # and `tilesmn$tileY` to be counted.
  # This is equivalent to say that I want the bounding box 
  # included inside the grid.
  grid <- expand.grid(c = 0:num_tiles_cols, r = 0:num_tiles_rows)

  # calculate tileX and tileY for each combination
  data <- grid |>
    dplyr::mutate(
      tileX = tilesmn$tileX + (.data$c * sign(resx)),
      tileY = tilesmn$tileY + (.data$r * sign(resy))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      quadkey = tileXY_to_quadkey(
        tileX = .data$tileX,
        tileY = .data$tileY,
        zoom = zoom
      )
    ) |>
    dplyr::ungroup() |> # remove rowwise grouping
    dplyr::select(-"c", -"r") # tidyselect

  return(list(
    data = data,
    num_rows = num_tiles_rows+1, #+1 as I was counting the zero
    num_cols = num_tiles_cols+1
  ))
}
