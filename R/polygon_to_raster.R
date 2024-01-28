#' Create and save raster images for different dates and times
#'
#' @description Creates one raster by each date and time reported and
#'  saves it as a .tif.
#'
#' @param template A spatial dataset (sf) with the polygon grid used as
#'  template
#' @param nx Integer; number of cells in x direction.
#' @param ny Integer; number of cells in y direction.
#' @param data A spatial dataframe (sf) with the variable we want to represent 
#' in the raster.
#' @param variable The column name of the variable to plot.
#' @param filename Select a name for the file. The date and time will
#'  be included automatically in the name.
#' @param path Path where the files should be stored.
#'
#' @seealso \code{\link{st_as_stars}}, \code{\link{st_rasterize}}
#' @seealso \code{\link{missing_combinations}}
#'
#' @importFrom rlang .data
#'  
#' @return as many .tif files as dates and times in the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' files <- read_fb_mobility_files(path_to_csvs = "../geocovid/data/rasters/city/",
#'  colnames = c("lat", "lon", 
#'             "quadkey", "date_time", 
#'             "n_crisis", "percent_change"),
#'. coltypes = list(
#'  lat = 'd',
#'  lon = 'd',
#'  quadkey = 'i',
#'  date_time = 'T',
#'  n_crisis = 'c',
#'  percent_change = 'c')) 
#'
#' # Extract unique QuadKey values
#' quadkeys <-  unique(files$quadkey)
#' 
#' # Convert QuadKeys to geographic coordinates
#' qtll <- quadkey_to_latlong(quadkeys = quadkeys)
#' 
#' # Complete the grid (if necessary)
#' regular_grid <- regular_qk_grid(qtll)
#' 
#' # Create polygons
#' polygrid  <-  grid_to_polygon(grid_coords)
#' 
#' # Keep only the QuadKeys reported
#' polyvar <- files |>
#'            dplyr::inner_join(polygrid, by = 'quadkey' )
#'
#' # Generate the raster files                       
#' polygon_to_raster(data = polyvar,
#'                   nx = grid$num_cols + 1,
#'                   ny = grid$num_rows + 1,
#'                   template = polyvar,
#'                   variable = 'percent_change',
#'                   filename = 'cityA',
#'                   path = "data/")
#' }
#'
polygon_to_raster <- function(data,
                              nx, ny,
                              template,
                              variable = 'percent_change',
                              filename,
                              path){
  
  # detect and reposrt combination of dates and times missing
  if (nrow(missing_combinations(data)) > 0) {
    warning("These combinations of days and times are missing in the dataset")
    missing_combinations(data)
  }
  
  
  days <- seq(from = min(data$day),
              to = max(data$day),
              by = 'days')
  
  mc <- missing_combinations(data)
  
  for(i in days){
    
    for(p in c(0, 8, 16)){
      
      # Is this iteration a missing file?
      no_data <- mc |>
        dplyr::filter(.data$day ==  as.Date(i, 
                                            origin = "1970-01-01") &
                        .data$time == p)
      
      # If it is, skip this iteration
      if (nrow(no_data) > 0) {
        next  # 
      }
      
      
      data <- data |>
        dplyr::filter(.data$day ==  as.Date(i, 
                                            origin = "1970-01-01") & 
                        .data$time == p)
      
      # Create raster using a template to avoid errors
      file <-  create_stars_raster(template = template,
                             nx = nx,
                             ny = ny,
                             data = data,
                             var = variable )
      
      # Save raster file
      stars::write_stars(obj = file,
                         dsn = paste0(path, 
                                      filename,
                                      "_",
                                      as.Date(i, origin = "1970-01-01"),
                                      "_",
                                      p,
                                      ".tif"))
      
    }}
}



