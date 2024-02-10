
 files <- read_fb_mobility_files(path_to_csvs = paste0(system.file("extdata",
                                                 package = "quadkeyr"), "/"),
                                 colnames = c("lat", "lon", 
                                              "quadkey", "date_time", 
                                              "n_crisis", "percent_change"),
                                 coltypes = list(
                                              lat = 'd',
                                              lon = 'd',
                                              quadkey = 'c',
                                              date_time = 'T',
                                              n_crisis = 'c',
                                              percent_change = 'c')) 

 # Get a regular grid and create the polygons
 regular_grid <- get_regular_polygon_grid(data = files)
 
 # Keep only the QuadKeys reported
 files_polygons <- files |> 
 dplyr::inner_join(regular_grid$data, 
                   by = c("quadkey"))


 test_that("polygon_to_raster produces output files", {
   
   # The files are saved in a temp directory for the tests
   withr::with_tempdir({
      
       path <- tempdir()
       
       # Generate the raster files                       
       polygon_to_raster(data = files_polygons,
                         nx = regular_grid$num_cols,
                         ny = regular_grid$num_rows,
                         template = files_polygons,
                         var = 'percent_change',
                         filename = 'cityA',
                         path = paste0(path, "/"))
       
       
       # Check if output files exist
       # These should be inside with_tempdir() as the files are removed
       # outside the function
       expect_true(file.exists(file.path(path, "cityA_2020-04-15_0.tif")))
       expect_true(file.exists(file.path(path, "cityA_2020-04-15_8.tif")))
       expect_true(file.exists(file.path(path, "cityA_2020-04-15_16.tif")))
     })

 })
 