#' Prepares the grid of QuadKeys for the conversion to square polygons
#'
#' @description The QuadKey's points of the grid represent the upper-left corner
#' of the QuadKey.
#' This function creates an extra tile row and column needed to create
#' the square polygons for the QuadKeys that are in the the borders of the area
#' without adding explicitly new QuadKeys to the grid.
#' This function is called internally by grid_to_polygon.
#'
#' @seealso \code{\link{grid_to_polygon}}
#'
#' @param data A spatial dataset (sf) with the columns tileX, tileY and quadkey.
#'
#' @importFrom rlang .data
#'
#' @return A spatial dataset (sf) with the columns tileX, tileY and quadkey.
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
#'   level = 11
#' )
#'
#' grid_coords <- get_qk_coord(data = grid$data)
#'
#'
#' # You can use grid_to_polygon
#'
#' extragrid <- complete_grid_for_polygons(grid_coords)
complete_grid_for_polygons <- function(data) {
  if (!("sf" %in% class(data))) {
    stop("The dataset should be of class 'sf'")
  }

  # I should add one row and one column to grid. To create the polygons
  # I am using other quadkeys (4 in total). I must complete the grid to can
  # convert the QuadKeys in the last line and last row to polygons

  textX <- max(data$tileX) + 1
  textY <- max(data$tileY) + 1
  level <- unique(nchar(data$quadkey))

  extragrid <- rbind(
    data.frame(
      tileX = seq(
        min(data$tileX),
        textX
      ),
      tileY = textY
    ),
    data.frame(
      tileX = textX,
      tileY = seq(
        min(data$tileY),
        textY - 1
      )
    )
  ) |>
    # the -1 is to avoid
    # duplicating the the point in the corner
    dplyr::mutate(quadkey = NA)
  # I am adding this points to fill the grid,
  # the quadkey value is not important here

  return(extragrid)
}


#' Convert a grid of QuadKeys to square polygons
#'
#' @description The main argument of this function, the grid of
#' geographic coordinates (lat/long WG84) represents the upper-left
#' corner of the QuadKey.
#' To transform these coordinates into square polygons, the function
#' supplements the grid by adding a row and column of tiles. These points
#' introduce QuadKeys located at the border of the area
#' (complete_grid_for_polygons).
#' The function constructs the polygons using all the points of the grid.
#' Note that it's possible to associate each QuadKey with its square polygon.
#'
#' @param data A spatial dataset (sf) with a quadkey and POINT geometry column.
#' If the columns tileX and tileY are included in the input dataset,
#' they will be used by complete_grid_for_polygons().
#' If not, the tile coordinates will be calculated internally.
#'
#' @seealso \code{\link{complete_grid_for_polygons}}
#'
#' @return A spatial dataset (sf) with the quadkey and POLYGON column.
#'
#' @export
#'
#' @examples
#' grid <- create_qk_grid(
#'   xmin = -59,
#'   xmax = -57,
#'   ymin = -35,
#'   ymax = -34,
#'   level = 11
#' )
#'
#' grid_coords <- get_qk_coord(data = grid$data)
#'
#' polygrid <- grid_to_polygon(grid_coords)
#' polygrid
grid_to_polygon <- function(data) {
  if (!("sf" %in% class(data))) {
    stop("The dataset should be of class 'sf'")
  }

  # Convert the QuadKeys to tile coordinates
  # if that columns aren't present in the data
  if (!("tileX" %in% colnames(data) | "tileY" %in% colnames(data))) {
    message(paste(
      "The 'tileX' and 'tileY' columns have been generated",
      "using the 'quadkey_to_tileXY' function."
    ))

    # In the case that one of this columns is not present, 
    # calculate both again.
    data$tileX <- NA
    data$tileY <- NA

    data <- data |>
      dplyr::rowwise() |>
      dplyr::mutate(
        tileX = quadkey_to_tileXY(.data$quadkey)$tileX,
        tileY = quadkey_to_tileXY(.data$quadkey)$tileY
      )
  }

  extragrid <- complete_grid_for_polygons(data)

  extragrid <- get_tile_coord(extragrid,
    level = unique(nchar(data$quadkey))
  )

  # combines the new data with the extended grid of points
  data <- rbind(data, extragrid)

  db <- c() # https://github.com/r-spatial/sf/issues/354

  # The QuadKeys of interest are the ones that are not NA:
  # the original QuadKeys of the grid
  subdata <- subset(data, !is.na(data$quadkey))

  for (i in seq_len(nrow(subdata))) {
    x <- subdata[i, ]$tileX
    y <- subdata[i, ]$tileY

    # This point will always be a QuadKey in the dataframe
    a <- data[data$tileX == x & data$tileY == y, ]

    # b, c and d can be part of the extended grid
    b <- data[data$tileX == x & data$tileY == (y + 1), ]

    c <- data[data$tileX == x + 1 & data$tileY == y, ]

    d <- data[data$tileX == x + 1 & data$tileY == y + 1, ]

    polygon <- rbind(a, b, c, d) |>
      sf::st_bbox() |>
      sf::st_as_sfc()

    grid_px <- sf::st_sf(
      quadkey = subdata[i, ]$quadkey,
      geometry = polygon,
      sf_column_name = "geometry"
    )

    db <- rbind(grid_px, db)
  }
  return(db)
}



#' Convert a QuadKey into a square polygon
#'
#' This functions creates a sf class polygon from a QuadKey.
#'
#' @param quadkey The QuadKey as a string
#'
#' @return A spatial dataset (sf) with a quadkey and POLYGON geometry column.
#' @export
#'
#' @examples
#' 
#' quadkey_to_polygon('213')
#' 
quadkey_to_polygon <- function(quadkey){
  
  tileX <- quadkey_to_tileXY(quadkey)$tileX
  tileY <- quadkey_to_tileXY(quadkey)$tileY
  
  # I add the other 3 points I need to complete the polygon
  # using the upper left coordinates of the closer tiles.
  polygon <- expand.grid(tileX = c(tileX, tileX+1),
              tileY = c(tileY, tileY+1)) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(pixelX = tileXY_to_pixelXY(.data$tileX, # conversion to coords
                                      .data$tileY)$pixelX,
           pixelY = tileXY_to_pixelXY(.data$tileX,
                                      .data$tileY)$pixelY) |> 
    dplyr::mutate(lat = pixelXY_to_latlong(.data$pixelX, 
                                           .data$pixelY,
                                           nchar(quadkey))$lat,
           lon = pixelXY_to_latlong(.data$pixelX, 
                                    .data$pixelY,
                                    nchar(quadkey))$lon) |> 
    sf::st_as_sf(coords = c("lon", "lat"), # class sf
             crs = 4326)
    
  # Create polygon https://github.com/r-spatial/sf/issues/243
 quadkey_polygon <-  polygon |>
    sf::st_bbox() |>
    sf::st_as_sfc() |> 
    sf::st_sf() |> 
    sf::st_set_geometry("geometry") |> 
    cbind(quadkey)
 
  return(quadkey_polygon)
}