
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
polygrid


# Check that the error is working when the data is not class 'sf'
test_that("Dataset is of class 'sf'", {
  expect_silent(polygrid)

  expect_error(grid_to_polygon(as.data.frame(grid_coords)),
    regexp = "The dataset should be of class 'sf'"
  )
})

test_that("tileX and tileY columns are created", {
  grid_coords_test <- subset(grid_coords,
    select = -c(tileX, tileY)
  )

  expect_message(
    grid_to_polygon(grid_coords_test),
    paste(
      "The 'tileX' and 'tileY' columns have been generated",
      "using the 'quadkey_to_tileXY' function."
    )
  )
})

test_that("quadkey_to_polygon gives the correct output", {
  polygon <- quadkey_to_polygon('213')
  expect_s3_class(polygon, "sf")
  expect_equal(polygon$quadkey, "213")
})
