# test conversion from latlong coordinates to pixel XY coordinate
test_that("latlong_to_pixelXY is giving correct results", {
  expect_equal(latlong_to_pixelXY(lat = -35,
                                  lon = -57,
                                  level = 11)$pixelX, 179132)
  expect_equal(latlong_to_pixelXY(lat = -35,
                                  lon = -57,
                                  level = 11)$pixelY, 316619)
})


# test conversion from pixel XY coordinates to tile XY coordinate
test_that("pixelXY_to_tileXYs is giving correct results", {
  expect_equal(pixelXY_to_tileXY(pixelX = 179132,
                                 pixelY = 316619)$tileX, 699)
  expect_equal(pixelXY_to_tileXY(pixelX = 179132,
                                 pixelY = 316619)$tileY, 1236)
})

# test conversion from tile XY coordinates to QuadKeys
test_that("tiles_to_quadkeys is giving correct results", {
  expect_equal(tileXY_to_quadkey(tileX = 699,
                                 tileY = 1236,
                                 level = 11), '21032131211')
})

# Check that errors appear in the correct cases - latlong_to_pixelXY

test_that("Check level detail - Negative level", {
  expect_error(latlong_to_pixelXY(lat = -35,
                                  lon = -57,
                                  level = -5),
               regexp = paste("The level of detail should be an integer",
                              "between 1 and 23"))
})

test_that("Check level detail - Greater than 23", {
  expect_error(latlong_to_pixelXY(lat = -35,
                                  lon = -57,
                                  level = 32),
               regexp = paste("The level of detail should be an integer",
                              "between 1 and 23"))
})

test_that("Check level detail - Use of decimals", {
  expect_error(latlong_to_pixelXY(lat = -35,
                                  lon = -57,
                                  level = 5.2),
               regexp = paste("The level of detail should be an integer",
                              "between 1 and 23"))
})

# Check that errors appear in the correct cases - tileXY_to_quadkey

test_that("Check level detail - Negative level", {
  expect_error(tileXY_to_quadkey(tileX = 23,
                                 tileY = 38,
                                 level = -6),
               regexp = paste("The level of detail should be an integer",
                              "between 1 and 23"))
})

test_that("Check level detail - Greater than 23", {
  expect_error(tileXY_to_quadkey(tileX = 23,
                                 tileY = 38,
                                 level = 77),
               regexp = paste("The level of detail should be an integer",
                              "between 1 and 23"))
})

test_that("Check level detail - Use of decimals", {
  expect_error(tileXY_to_quadkey(tileX = 23,
                                 tileY = 38,
                                 level = 7.2),
               regexp = paste("The level of detail should be an integer",
                              "between 1 and 23"))
})

# latlong_to_quadkey

test_that("I got the correct QuadKey with latlong_to_quadkey", {
  expect_equal(latlong_to_quadkey(lat = 35.63051,
                                  lon = 139.69116,
                                  level = 20)$quadkey[1], 
               '13300211230321211111')
})

test_that("I got the correct coordinates with quadkey_to_latlong", {
  
  qk <- '13300211230321211111'
  
  expect_equal(sf::st_coordinates(quadkey_to_latlong(qk)$geometry), 
               matrix(c(139.6908, 35.63051),
                      nrow = 1, 
                      byrow = TRUE, 
                      dimnames = list(NULL, c("X", "Y"))), 
               tolerance = 1.04e-05)
})
