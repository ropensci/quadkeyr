#' Converts a irregular QuadKey grid into a regular grid
#'
#' @description This function completes the grid of QuadKeys using the bounding
#' box of the data provided.
#'
#' @param data A spatial dataset of class sf with POINT geometry
#'
#' @seealso \code{\link{create_qk_grid}}
#' @seealso \code{\link{quadkey_to_latlong}}
#'
#' @return A spatial dataset of class sf with POINT geometry
#' @export
#'
#' @examples
#'
#' quadkeys <- c("213", "210", "211")
#'
#' qtll <- quadkey_to_latlong(quadkeys = quadkeys)
#'
#' regular_qk_grid(qtll)
#'
regular_qk_grid <- function(data) {
  # I need the data.frame to be class simple features to can 
  # estimate the bounding box
  bbox <- sf::st_bbox(data) 

  # I assume that the all the QuadKeys correspond to the same zoom level.
  qk_zoom <- nchar(data$quadkey[1])

  # Create the complete grid
  grid <- create_qk_grid(
    xmin = bbox$xmin[[1]],
    xmax = bbox$xmax[[1]],
    ymin = bbox$ymin[[1]],
    ymax = bbox$ymax[[1]],
    zoom = qk_zoom
  )


  if (nrow(grid$data) == nrow(data)) {
    warning("The grid is already complete, this function is not necessary")
    return(data)
  }

  # Select the QuadKeys that are missing in the original grid
  qk_missing <- grid$data |>
    dplyr::anti_join(data, by = "quadkey") |>  #tidyselect
    dplyr::select(-"tileX", -"tileY") # tidyselect
  
  # Convert the QuadKeys to coordinates
  grid_coords <- quadkey_to_latlong(qk_missing$quadkey)

  
  if(ncol(grid_coords) == ncol(data)){
  # Add the missing QuadKey coordinates to the original dataset
  data <- rbind(grid_coords, data)
  }else{
    # this function will introduce NAs if there are columns 
    # different than quadkey and geometry
    # probably used inside add_regular_polygon_grid()
  data <- dplyr::bind_rows(grid_coords, data)
  }

  return(list(
    data = data,
    num_rows = grid$num_rows,
    num_cols = grid$num_cols
  ))
}



#' Complete a data.frame with the quadkeys needed to have a regular grid.
#'
#' @description
#' This function estimates the bounding box of the quadkeys given in the 
#' quadkey column and adds rows to complete the quadkeys and the geometry 
#' needed to create a regular grid.
#' All other columns for the introduced quadkeys will be filled with NAs.
#' 
#' @param data A data.frame with a quadkey column.
#' 
#' @return An sf POLYGON data.frame with all the quadkeys within
#' the bounding box of the provided quadkeys.
#' The columns quadkey and geometry are returned for all the grid,
#' and the values of the introduced quadkeys will be NA
#' for the rest pf the variables.
#'  
#' @export
#'
#' @examples
#' # read the file with the data
#' path <- paste0(system.file("extdata", package = 'quadkeyr'),
#'                              "/cityA_2020_04_15_0000.csv")
#' data <- read.csv(path)
#' data <- format_fb_data(data)
#' 
#' get_regular_polygon_grid(data = data)
add_regular_polygon_grid <- function(data){
   sf_grid <- get_qk_coord(data)
   reggrid <- regular_qk_grid(sf_grid)
   add_polygrid <- grid_to_polygon(reggrid$data)
   return(list(data = add_polygrid,
          num_cols = reggrid$num_cols,
          num_rows = reggrid$num_rows))

}
  

#' Create a data.frame with all the quadkeys needed to
#' generate a regular grid.
#' 
#' @description
#' This function estimates the bounding box of the quadkeys given in the 
#' quadkey column and adds rows to complete the quadkeys and the geometry 
#' needed to create a regular grid.
#'
#' @param data A data.frame with a quadkey column.
#'
#' @return An sf POLYGON data.frame with all the quadkeys within
#' the bounding box of the provided quadkeys.
#' Only the columns quadkey and geometry are returned.
#' 
#' @export
#'
#' @examples
#' 
#' # read the file with the data
#' path <- paste0(system.file("extdata", package = 'quadkeyr'),
#'                              "/cityA_2020_04_15_0000.csv")
#' data <- read.csv(path)
#' data <- format_fb_data(data)
#' 
#' get_regular_polygon_grid(data = data)
get_regular_polygon_grid <- function(data){

  # I convert the QuadKeys to points and not polygons directly
  # Because I want to generate the regular grid first 
  grid_coords <- quadkey_to_latlong(quadkeys = unique(data$quadkey))
  reggrid <- regular_qk_grid(grid_coords)
  polygrid <- grid_to_polygon(reggrid$data)
  return(list(data = polygrid,
         num_cols = reggrid$num_cols,
         num_rows = reggrid$num_rows))
}

