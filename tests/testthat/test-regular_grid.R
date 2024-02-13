# Mock data
data <- quadkey_to_latlong(quadkey = c("213", "312", "123"))
result <- regular_qk_grid(data)

# Test cases for `regular_qk_grid()`
test_that("regular_qk_grid creates the expected dataframe", {
  # Check the structure of the output
  expect_s3_class(result$data, "sf")
  expect_equal(result$num_rows, 3)
  expect_equal(result$num_cols, 4)
  
  # Check if output data frame has expected columns
  expected_cols <- c("quadkey", "geometry")
  expect_equal(colnames(result$data), expected_cols,
               info = "Output data frame should have expected columns")
  
  # Check if the warning appears
  expect_warning(
    regular_qk_grid(result$data),
    paste("The grid is already complete,",
          "this function is not necessary")
  )
})

# Test cases for `get_regular_grid()`
files <- read_fb_mobility_files(
  path_to_csvs = paste0(system.file("extdata",
                                    package = "quadkeyr"), "/"),
  colnames = c(
    "lat",
    "lon",
    "quadkey",
    "date_time",
    "n_crisis",
    "percent_change"
  ),
  coltypes = list(
    lat = 'd',
    lon = 'd',
    quadkey = 'c',
    date_time = 'T',
    n_crisis = 'c',
    percent_change = 'c'
  )
)

regular_grid <- get_regular_polygon_grid(data = files)

test_that("`get_regular_polygon_grid()` returns the expected output structure",
          {
            # Check the structure of the output
            # Check if the result is a list
            expect_type(regular_grid, "list")
            
            # Check if the list contains necessary elements
            expect_named(regular_grid, c("data", "num_cols", "num_rows"))
            
            # Check if the data element is a data.frame
            expect_s3_class(regular_grid$data, "data.frame")
            expect_s3_class(regular_grid$data, "sf")
            
            # Check if num_cols and num_rows are numbers
            expect_type(regular_grid$num_cols, "double")
            expect_type(regular_grid$num_rows, "double")
          })

test_that("`get_regular_polygon_grid()` returns the expected # of columns and rows",
          {
            # Check if the number of cols and rows matches the expected values
            expect_equal(regular_grid$num_cols, 18)
            expect_equal(regular_grid$num_rows, 22)
          })


test_that("`get_regular_polygon_grid()` is providing a complete grid", {
  # Test 1: The QuadKeys are in relation to the grid dimensions
  expect_equal((regular_grid$num_cols) * (regular_grid$num_rows),
               length(regular_grid$data$quadkey)
  )
})

# Test cases for `add_regular_polygon_grid()`
added_regular_grid <- add_regular_polygon_grid(data = files)

test_that("`add_regular_polygon_grid()` returns the expected output structure",
          {
            # Check if the result is a list
            expect_type(added_regular_grid, "list")
            
            # Check if the list contains necessary elements
            expect_named(regular_grid, c("data", "num_cols", "num_rows"))
            
            # Check if the data element is a data frame
            expect_s3_class(added_regular_grid$data, "data.frame")
            expect_s3_class(added_regular_grid$data, "sf")
            
            # Check if num_cols and num_rows are numeric
            expect_type(added_regular_grid$num_cols, "double")
            expect_type(added_regular_grid$num_rows, "double")
          })

test_that("`add_regular_polygon_grid()` returns the expected # of col and rows",
          {
            # Check if the number of cols and rows matches the expected values
            expect_equal(added_regular_grid$num_cols, 18)
            expect_equal(added_regular_grid$num_rows, 22)
          })

test_that("`add_regular_polygon_grid()` is returning NA when adding QuadKeys",
          {
            # Expected NAs are present
            sf::st_geometry(added_regular_grid$data) <- NULL
            data_test <- added_regular_grid$data
            expect_true(is.na(data_test[data_test$quadkey == "2103213001320121",
                                        'date_time'])[1])
          })
