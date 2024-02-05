# example
grid <- create_qk_grid(
  xmin = -59,
  xmax = -57,
  ymin = -35,
  ymax = -34,
  zoom = 11
)

grid_coords <- get_qk_coord(data = grid$data)
grid_coords

polygrid <- grid_to_polygon(grid_coords)

data <- polygrid |>
  dplyr::mutate(var = runif(nrow(polygrid)))


test_that("create_raster function creates raster as expected", {
  # Test 1: Check if the returned object is a stars object
  result <- create_stars_raster(data,
    nx = grid$num_cols + 1,
    ny = grid$num_rows + 1,
    data,
    "var"
  )
  expect_true(is(result, "stars"),
    info = "Output should be a stars object"
  )

  # Test 2: Check if the number of rows and columns match the specified nx, ny
  expect_equal(c(dim(result)[[1]], dim(result)[[2]]),
    c(grid$num_cols + 1, grid$num_rows + 1),
    info = "Resulting raster dimensions should match nx and ny"
  )
})
