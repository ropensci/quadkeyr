# test conversion from latlong coordinates to pixel XY coordinate
test_that("latlong_to_pixelXY is giving correct results", {
  expect_equal(latlong_to_pixelXY(
    lat = -35,
    lon = -57,
    zoom = 11
  )$pixelX, 179132)
  expect_equal(latlong_to_pixelXY(
    lat = -35,
    lon = -57,
    zoom = 11
  )$pixelY, 316619)
})


# test conversion from pixel XY coordinates to tile XY coordinate
test_that("pixelXY_to_tileXYs is giving correct results", {
  expect_equal(pixelXY_to_tileXY(
    pixelX = 179132,
    pixelY = 316619
  )$tileX, 699)
  expect_equal(pixelXY_to_tileXY(
    pixelX = 179132,
    pixelY = 316619
  )$tileY, 1236)
})

# test conversion from tile XY coordinates to QuadKeys
test_that("tiles_to_quadkeys is giving correct results", {
  # Test with valid zoom, tileX, and tileY values
  result_valid <- tileXY_to_quadkey(3, 5, 3)
  expect_is(result_valid, "character")

  expect_equal(tileXY_to_quadkey(
    tileX = 699,
    tileY = 1236,
    zoom = 11
  ), "21032131211")
})

test_that("tileXY_to_quadkey handles invalid tileX and tileY values", {
  # Test with invalid tileX and tileY values
  expect_error(tileXY_to_quadkey(-1, 5, 2), "Invalid tileX or tileY values.")
  expect_error(tileXY_to_quadkey(3, 10, 2), "Invalid tileX or tileY values.")
  expect_error(tileXY_to_quadkey(3, -5, 2), "Invalid tileX or tileY values.")
})


# Check that errors appear in the correct cases - latlong_to_pixelXY

test_that("Check zoom level - Negative value", {
  expect_error(latlong_to_pixelXY(
    lat = -35,
    lon = -57,
    zoom = -5
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

test_that("Check the zoom level - Greater than 23", {
  expect_error(latlong_to_pixelXY(
    lat = -35,
    lon = -57,
    zoom = 32
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

test_that("Check the zoom level - Use of decimals", {
  expect_error(latlong_to_pixelXY(
    lat = -35,
    lon = -57,
    zoom = 5.2
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

# Check that errors appear in the correct cases - tileXY_to_quadkey

test_that("Check zoom level - Negative value", {
  expect_error(tileXY_to_quadkey(
    tileX = 23,
    tileY = 38,
    zoom = -6
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

test_that("Check zoom level - Greater than 23", {
  expect_error(tileXY_to_quadkey(
    tileX = 23,
    tileY = 38,
    zoom = 77
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

test_that("Check zoom level - Use of decimals", {
  expect_error(tileXY_to_quadkey(
    tileX = 23,
    tileY = 38,
    zoom = 7.2
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

# latlong_to_quadkey

test_that("I got the correct QuadKey with latlong_to_quadkey", {
  expect_equal(
    latlong_to_quadkey(
      lat = 35.63051,
      lon = 139.69116,
      zoom = 20
    )$quadkey[1],
    "13300211230321211111"
  )
})

test_that("I got the correct coordinates with quadkey_to_latlong", {
  qk <- "13300211230321211111"

  expect_equal(sf::st_coordinates(quadkey_to_latlong(qk)$geometry),
    matrix(c(139.6908, 35.63051),
      nrow = 1,
      byrow = TRUE,
      dimnames = list(NULL, c("X", "Y"))
    ),
    tolerance = 1.04e-05
  )
})
