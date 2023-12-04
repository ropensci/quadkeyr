#' Create grid of QuadKeys for a particular level of detail.
#'
#' @description Generates a grid comprising all the QuadKeys within the area
#' defined by the maximum and minimum coordinates of latitude and
#' longitude along with a specified level of detail.
#'
#'
#' @param xmin Minimum value in the x axis (longitude)
#' @param xmax Maximum value in the y axis (latitude)
#' @param ymin Minimum value in the x axis (longitude)
#' @param ymax Maximum value in the Y axis (latitude)
#' @param level Level of detail, from 1 (lowest detail) to 23 (highest detail).
#'
#' @return A list returning the QuadKeys as a dataframe (data),
#' the number of rows (num_rows)
#' and columns (num_cols) of the grid.
#'
#' @export
#'
#' @examples
#'
#' grid = create_qk_grid( xmin = -59,
#'                        xmax = -57,
#'                        ymin = -35,
#'                        ymax = -34,
#'                        level = 12)
#'
create_qk_grid <- function(xmin, xmax, ymin, ymax, level){

  if (level < 0 | level > 23 | (level %% 1) != 0){
    stop("The level of detail should be an integer between 1 and 23")
  }

  # Values in Microsoft Bing Documentation
  min_latitude = -85.05112878
  max_latitude = 85.05112878
  min_longitude = -180
  max_longitude = 180


   # this variables were defined in the function ground_res
  if (ymin < min_latitude || ymax > max_latitude ||
      xmin < min_longitude || xmax > max_longitude) {
    stop(paste("At least one of the provided coordinates are outside the valid range.",
               "Latitude must be between -85.05112878 and 85.05112878.",
               "Longitude must be between -180 and 180."))
  }



  # x - Convert lat/long coordinates to tile XY coords
  pixs = latlong_to_pixelXY(latitude = ymin,
                          longitude = xmin,
                          level = level)
  tilesmn = pixelXY_to_tileXY(pixelX = pixs$pixelX,
                          pixelY = pixs$pixelY)


  # y - Convert lat/long coordinates to tile XY coords
  pixs = latlong_to_pixelXY(latitude = ymax,
                          longitude = xmax,
                          level = level)
  tilesmx = pixelXY_to_tileXY(pixelX = pixs$pixelX,
                          pixelY = pixs$pixelY)

  # How many tile XY coordinates conform the grid?
  resy = tilesmx$tileY - tilesmn$tileY
  resx = tilesmx$tileX - tilesmn$tileX

  if(resx == 0 | resy == 0){
    stop(paste(
      "The selected inputs fail to generate a grid due to the limited area",
      "for this level of detail. Consider adjusting the level of detail",
      "or modifying the xmin, xmax, ymin, or ymax values."
    ))
  }

  # define the dimensions of the matrix
  num_rows <- abs(resy)
  num_cols <- abs(resx)

  # create the grid with all the possible combination of tile XY coordinates
  data = c()
  for(c in 0:num_cols){ # I consider 0 as the point provided should be included
    for(r in 0:num_rows){

     grid = data.frame(tileX = tilesmn$tileX + (c * sign(resx)),
                 tileY = tilesmn$tileY + (r * sign(resy))) |>
                   dplyr::mutate(quadkey = tileXY_to_quadkey(tileX = tileX,
                                           tileY = tileY,
                                           level = level))

     data = rbind(data, grid)

    }}


return(list(data = data,
            num_rows =  num_rows,
            num_cols =  num_cols))

}
