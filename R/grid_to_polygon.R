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
#' @return A spatial dataset (sf) with the columns tileX, tileY and quadkey.
#' @export
#'
#' @examples
#'
#' grid = create_qk_grid(xmin = -59,
#'                       xmax = -57,
#'                       ymin = -35,
#'                       ymax = -34,
#'                       level = 11)
#'
#' grid_coords <- extract_qk_coord(data = grid$data)
#'
#'
#' # You can use grid_to_polygon
#'
#' extragrid <- complete_grid_for_polygons(grid_coords)
#'
#'
complete_grid_for_polygons <- function(data){

  if (!("sf" %in% class(data))) {
    stop("The dataset should be of class 'sf'")
  }

  # I should add one row and one column to grid. To create the polygons I am using
  # other quadkeys (4 in total). I. must complete the grid to can convert the
  # quadkeys in the last line and last row to polygons

  textX = max(data$tileX) + 1
  textY = max(data$tileY) + 1
  level = unique(nchar(data$quadkey))

  extragrid =  rbind(
    data.frame(
    tileX = seq(min(data$tileX),
                         textX),
    tileY = textY),
    data.frame(tileX = textX,
               tileY = seq(min(data$tileY),
                                    textY -1))) |> # there should be -1 to not duplicate the point in the corner
    dplyr::mutate(quadkey = NA) # I am adding this points to fill the grid, the qk value is not important here

}


#' Convert a grid of QuadKeys to square polygons
#'
#' @description The main argument of this function, the grid of QuadKeys points
#' representing lat/long WG84 coordinates specifically indicate the upper-left corner of the QuadKey.
#' To transform these coordinates into square polygons, the function
#' supplements the grid by adding a row and column of tiles. This completion of the
#' grid addresses QuadKeys located at the border of the area (complete_grid_for_polygons).
#' The function constructs the polygons using all the points of the grid.
#' Note that it's possible to associate each QuadKey with its square polygon.
#'
#' @param data A spatial dataset (sf) with a quadkey and POINT geometry column.
#'
#'
#' @seealso \code{\link{complete_grid_for_polygons}}
#'
#' @return A spatial dataset (sf) with the quadkey and POLYGON column.
#' @export
#'
#' @examples
#' grid = create_qk_grid(xmin = -59,
#'                       xmax = -57,
#'                       ymin = -35,
#'                       ymax = -34,
#'                       level = 11)
#'
#'                       grid_coords <- extract_qk_coord(data = grid$data)
#'                       grid_coords
#'
#'                       polygrid = grid_to_polygon(grid_coords)
#'                       polygrid
grid_to_polygon <- function(data){

  if (!("sf" %in% class(data))) {
    stop("The dataset should be of class 'sf'")
  }

  extragrid <- complete_grid_for_polygons(data)

  extragrid <- extract_tile_coord(extragrid, level = unique(nchar(data$quadkey)))

   # combines the new data with the extended grid oof points
  data = rbind(data, extragrid)

  db = c() #https://github.com/r-spatial/sf/issues/354

  subdata = subset(data, !is.na(data$quadkey))

  for(i in 1:nrow(subdata)){

    # podria probar que tengo todos los tiles que necesito y si no los deberia crear

    x = subdata[i, ]$tileX
    y =  subdata[i, ]$tileY

  # This point will always exists
  a = data |>
    dplyr::filter(.data$tileX == x & .data$tileY == y)

  b = data |>
    dplyr::filter(.data$tileX == x & .data$tileY == (y + 1))

  c = data |>
    dplyr::filter(.data$tileX == x + 1  & .data$tileY == y)

  d = data |>
    dplyr::filter(.data$tileX == x + 1 & .data$tileY == y + 1)

  pixel = rbind(a, b, c, d) |>
        sf::st_bbox() |>
        sf::st_as_sfc()

   grid_px =  sf::st_sf(quadkey = subdata[i, ]$quadkey,
                        geometry = pixel,
                        sf_column_name = 'geometry')

   db = rbind(grid_px, db)
  #st_write(pixel, 'pixel.gpkg', append = TRUE)

  }
  return(db)
}



#   # tiene que haber un minimo de puntos
#
#   # algunos cambos en la data
#   datapre = data |> st_transform(3857) # only if it is sf
#   data = datapre |>
#     cbind(st_coordinates(datapre))
# print(data)
#
#
#
#     # elijo los 4 mas cercanos
#     pol = data[i,] |>
#       st_is_within_distance(data, dist = ratio) #2500 para baires y 750 para amba
#
#
#
#     # consigo las coordenadas de esos 4 mas cercanos
#     subpol = data[pol[[1]], ]
#
#     # extraigo la bbox
#     bb = st_bbox(data[pol[[1]], ])
#
#     # elimino puntos que no van
#     vv <-  subpol |>
#       filter(X != bb$xmin, Y != bb$ymax)
#
#     # Si hay menos de 3 es que no detecto suficientes puntos
#     if(nrow(vv) == 3){
#
#       # detecto el numero de quadkey para asignarlo al pixel
#       qk <- vv |>  filter(Y != bb$ymin, X != bb$xmax)
#
#       # selecciono puntos que cierran el pixel
#       pts_cmpt = c(bb$xmax[[1]], bb$ymin[[1]])
#
#      st_geometry(vv) <-  NULL
#      print(vv)
#      pts_pixl = vv |>  select(X, Y) |>
#         rbind(data.frame(X = pts_cmpt[1],
#                          Y = pts_cmpt[2])) |>
#         mutate(quadkey = qk$quadkey) |>
#        st_as_sf(coords = c('X', "Y")) # convierto a sf
#
#
#      print(pts_pixl)
#
#       pixel = pts_pixl |>
#         group_by(quadkey) |>
#         summarize() |>
#         st_cast('POLYGON') |>
#         st_convex_hull() # pixel!
#
#       print(pixel)
#
#       db = rbind(pixel, db) }
#
#   }
#   print(db)
#
#
# }
