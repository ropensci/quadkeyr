#' Convert a grid of QuadKeys to square polygons
#'
#' @description The main argument of this function, the grid of
#' geographic coordinates (lat/long WG84) represents the upper-left
#' corner of the QuadKey.
#' To transform these coordinates into square polygons, the function
#' supplements the grid by adding a row and column of tiles. These points
#' introduce QuadKeys located at the border of the area using the internal 
#' function `complete_grid_for_polygons()`.
#' The function builds the polygons using all the points of the grid.
#' Note that it's possible to associate each QuadKey with its square polygon.
#'
#' @param data A `sf` POINT data.frame with a `quadkey` and `geometry` column.
#'
#' @return A sf POLYGON data.frame with a quadkey column.
#'
#' @export
#' @examples
#' grid <- create_qk_grid(
#'   xmin = -59,
#'   xmax = -57,
#'   ymin = -35,
#'   ymax = -34,
#'   zoom = 11
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


  # We add the extra row and column we need for the grid and
  # Convert it to an sf POINT data.frame
  extragrid <- complete_grid_for_polygons(data)
  extragrid <- get_tile_coord(extragrid,
    zoom = unique(nchar(data$quadkey))
  )

  # Now we can combine it with the original data.
  if (ncol(data) == ncol(extragrid)) {
    # combines the new data with the extended grid of points
    data <- rbind(data, extragrid)
  } else {
    # In the case that there are more columns in data,
    # keep the rows but adding NAs in the extragrid cells.
    data <- dplyr::bind_rows(data, extragrid)
  }

  polydata <- c() # https://github.com/r-spatial/sf/issues/354

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

    subdata_row <- subdata[i, ] |>
      dplyr::mutate(geometry = sf::st_sfc(polygon)) |>
      sf::st_set_geometry("geometry")

    # grid_px <- sf::st_sf(
    #   quadkey = subdata[i, ]$quadkey,
    #   geometry = polygon,
    #   sf_column_name = "geometry"
    # )

    polydata <- rbind(subdata_row, polydata)
  }

  return(polydata)
}

#' Prepare the grid of QuadKeys for the conversion to square polygons
#'
#' @description The QuadKey's points of the grid represent
#' the upper-left corner of the QuadKey.
#' This function creates an extra tile row and column needed to create
#' the square polygons for the QuadKeys 
#' that are in the the borders of the area
#' without adding explicitly new QuadKeys to the grid.
#' This function is called internally by [grid_to_polygon()].
#'
#' @seealso \code{\link{grid_to_polygon}}
#'
#' @param data A `sf` POINT data.frame with the `quadkey`,
#' `tileX`, `tileY` and `geometry` columns
#'
#' @importFrom rlang .data
#'
#' @return A data.frame with the `tileX`, `tileY` and `quadkey` columns.
#' QuadKeys appears as NAs as there are only kept to extend the grid.
#' Their value is not necessary.
#'
#' @noRd
complete_grid_for_polygons <- function(data) {
  if (!("sf" %in% class(data))) {
    stop("The dataset should be of class 'sf'")
  }

  # I should add one row and one column to grid. To create the polygons
  # I am using other QuadKeys (4 in total). I must complete the grid to can
  # convert the QuadKeys in the last line and last row to polygons

  textX <- max(data$tileX) + 1
  textY <- max(data$tileY) + 1
  zoom <- unique(nchar(data$quadkey))

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
  # I am adding these points to fill the grid,
  # the QuadKey value is not important here

  return(extragrid)
}
