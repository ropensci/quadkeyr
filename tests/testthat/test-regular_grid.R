# Mock data
data <- quadkey_to_latlong(quadkey = c("213", "312", "123"))
result <- regular_qk_grid(data)

# Test cases for regular_qk_grid function
test_that("regular_qk_grid creates the expected dataframe", {

  # Test 1: Check the structure of the output

  expect_is(result$data, "sf",
    info = "Output 'data' should be a sf dataset"
  )
  expect_equal(result$num_rows, 2,
    info = "Number of rows in grid should be 3"
  )
  expect_equal(result$num_cols, 3,
    info = "Number of columns in grid should be 3"
  )

  # Test 2: Check if output data frame has expected columns
  expected_cols <- c("quadkey", "geometry", "tileX", "tileY")
  expect_equal(colnames(result$data), expected_cols,
    info = "Output data frame should have expected columns"
  )
  expect_warning(
    regular_qk_grid(result$data),
    paste(
      "The grid is already complete,",
      "this function is not necessary"
    )
  )
})
