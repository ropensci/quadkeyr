#' Create a stars raster
#'
#' @description The use of a template enables the creation of an accurate 
#' raster, even in the presence of NAs.
#'
#' @param template A spatial dataset (sf) with the polygon grid used as template
#' @param nx Integer; number of cells in x direction.
#' @param ny Integer; number of cells in y direction.
#' @param data A spatial dataframe (sf) with the variable we want to represent 
#' in the raster.
#' @param var The column name of the variable to plot.
#'
#' @seealso \code{\link{st_as_stars}}, \code{\link{st_rasterize}}
#'
#' @return A stars object
#' @export
#'
#' @examples
#'
#'  grid = create_qk_grid(
#'                      xmin = -59,
#'                      xmax = -57,
#'                      ymin = -35,
#'                      ymax = -34,
#'                      level = 12)
#'
#'  grid_coords <- extract_qk_coord(data = grid$data)
#'
#'  polygrid <- grid_to_polygon(grid_coords)
#'
#'  data('data_provided')
#'
#'  data_raster <-  polygrid |>
#'                    dplyr::inner_join(data_provided,
#'                     by = c('quadkey' ))
#'
#'  raster <-  create_raster(template = data_raster,
#'                          nx = grid$num_cols + 1,
#'                          ny = grid$num_rows + 1,
#'                          data = data_raster,
#'                          var = 'variable')
#'
#'
create_raster <- function(template,
                          nx, ny,
                          data,
                          var){

raster_tmplt <- stars::st_as_stars(sf::st_bbox(template),
                                    values = NA_real_,
                                   ny = ny,
                                   nx = nx)

r <- stars::st_rasterize(data[,as.character(var)],
                 template = raster_tmplt)

return(r)

}
