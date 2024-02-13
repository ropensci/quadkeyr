# Test cases for `quadkey_to_polygon()`

test_that("`quadkey_to_polygon()` gives the correct output", {
  polygon <- quadkey_to_polygon('213')
  expect_s3_class(polygon, "sf")
  expect_equal(polygon$quadkey, "213")
})

# Test cases for `quadkey_df_to_polygon()`

# example
grid <- create_qk_grid(
  xmin = -59,
  xmax = -57,
  ymin = -35,
  ymax = -34,
  zoom = 12
)

test_that("`quadkey_df_to_polygon()` errors are correct", {
  data <- grid$data
  # If quadkey is not a character column, it should stop
  data$quadkey <- as.numeric(data$quadkey)
  expect_error(quadkey_df_to_polygon(data = data),
               "quadkey should be a character column")
  
  # If QuadKeys have different number of digits,
  # it should stop with appropriate message
  data$quadkey[1] <- "123"
  expect_error(
    quadkey_df_to_polygon(data = data),
    "All the QuadKeys should have the same number of digits"
  )
  
  # If QuadKeys contain invalid characters,
  # it should stop with appropriate message
  data$quadkey[1] <- "11112222333a"
  expect_error(
    quadkey_df_to_polygon(data = data),
    "QuadKeys can contain only the numbers '0', '1', '2', or '3'"
  )
})

test_that("`quadkey_df_to_polygon()` error tests", {
  data <- grid$data[1:3,]
  # Test that the function runs without errors with correct quadkey format
  expect_silent(quadkey_df_to_polygon(data))
  
  # Create a sample data frame with incorrect quadkey format
  # (different number of digits)
  data$quadkey <- as.numeric(data$quadkey)
  # Test that the function throws an error
  expect_error(quadkey_df_to_polygon(data),
               "quadkey should be a character column")
  
  # Create a sample data frame with incorrect quadkey format
  # (different number of digits)
  data <- grid$data[1:3,]
  data$quadkey[1] <- "123"
  # Test that the function throws an error
  expect_error(quadkey_df_to_polygon(data),
               "All the QuadKeys should have the same number of digits")
  # Create a sample data frame with incorrect quadkey format
  # (invalid characters)
  data$quadkey[1] <- "444455556666"
  # Test that the function throws an error
  expect_error(
    quadkey_df_to_polygon(data),
    "QuadKeys can contain only the numbers '0', '1', '2', or '3'"
  )
  # Create a sample data frame with incorrect quadkey format
  # (invalid characters)
  data$quadkey[1] <- "11112222333e"
  # Test that the function throws an error
  expect_error(
    quadkey_df_to_polygon(data),
    "QuadKeys can contain only the numbers '0', '1', '2', or '3'"
  )
})


