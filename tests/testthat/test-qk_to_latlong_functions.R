# Test QuadKey to tile XY coordinates conversion

test_that("`quadkey_to_tileXY()` is giving correct results", {
  expect_equal(quadkey_to_tileXY("213")$tileX, 3)
  expect_equal(quadkey_to_tileXY("213")$tileY, 5)
  expect_equal(quadkey_to_tileXY("213")$zoom, 3)
  expect_equal(quadkey_to_tileXY('0313102310')$tileX, 486)
  expect_equal(quadkey_to_tileXY('0313102310')$tileY, 332)
  expect_equal(quadkey_to_tileXY('0313102310')$zoom, 10)
})

test_that("Error: QuadKeys should be strings", {
  # Test with a character vector
  expect_silent(quadkey_to_tileXY("322"))
  # Test with a number
  expect_error(quadkey_to_tileXY(322), 
               "Please provide a QuadKey as a single string")
  # Test with a vector
  expect_error(quadkey_to_tileXY(c("322", "123")), 
               "Please provide a QuadKey as a single string")
})

test_that("Error: QuadKeys contain only the numbers '0', '1', '2', or '3'", {
  # Test with valid input
  valid_quadkey <- "0123"
  expect_silent(quadkey_to_tileXY(valid_quadkey))
  
  # Test with invalid input character
  invalid_quadkey <- "123f"
  expect_error(quadkey_to_tileXY(invalid_quadkey), 
               "QuadKeys can contain only the numbers '0', '1', '2', or '3'")
  # Test with invalid input number
  invalid_quadkey2 <- "1234"
  expect_error(quadkey_to_tileXY(invalid_quadkey2), 
               "QuadKeys can contain only the numbers '0', '1', '2', or '3'")
})

# Test tile XY coordinates to pixel XY coordinates
test_that("tileXY_to_pixelXY is giving correct results", {
  expect_equal(tileXY_to_pixelXY(
    tileX = 3,
    tileY = 5
  )$pixelX, 768)
  expect_equal(tileXY_to_pixelXY(
    tileX = 3,
    tileY = 5
  )$pixelY, 1280)
})


# Test pixels XY coordinates to map coordinates WG84 in degrees

test_that("`pixelXY_to_latlong()` is giving correct results", {
  expect_equal(round(pixelXY_to_latlong(
    pixelX = 768,
    pixelY = 1280,
    zoom = 3
  )$lat, digits = 2), -40.98)
  expect_equal(round(pixelXY_to_latlong(
    pixelX = 768,
    pixelY = 1280,
    zoom = 3
  )$lon, digits = 2), -45)
  # Test with valid zoom and pixel values
  result_valid <- pixelXY_to_latlong(100, 150, 5)
  expect_is(result_valid, "list")
  expect_length(result_valid, 2)
})

# Check that errors appear in the correct cases - `pixelXY_to_latlong()`

test_that("Check zoom level - Negative value", {
  expect_error(pixelXY_to_latlong(
    pixelX = 768,
    pixelY = 1280,
    zoom = -3
  ),
  regexp = paste(
    "The zoom level should be",
    "an integer between 1 and 23"
  )
  )
})

test_that("Check zoom detail - Greater than 23", {
  expect_error(pixelXY_to_latlong(
    pixelX = 768,
    pixelY = 1280,
    zoom = 44
  ),
  regexp = paste(
    "The zoom level should be",
    "an integer between 1 and 23"
  )
  )
})

test_that("Check zoom level - Use of decimals", {
  expect_error(pixelXY_to_latlong(
    pixelX = 768,
    pixelY = 1280,
    zoom = 4.7
  ),
  regexp = paste(
    "The zoom level should be",
    "an integer between 1 and 23"
  )
  )
})

test_that("`pixelXY_to_latlong()` handles invalid pixelX and pixelY values", {
  # Test with invalid pixelX and pixelY values
  expect_error(
    pixelXY_to_latlong(-1, 20, 3),
    "Invalid pixelX or pixelY values."
  )
  expect_error(
    pixelXY_to_latlong(0, 3000, 3),
    "Invalid pixelX or pixelY values."
  )
})


# Test cases for `quadkey_to_latlong()`
test_that("`quadkey_to_latlong()` function works as expected", {
  quadkeys <- c("213", "312", "213")

  # Check for duplicated quadkeys
  expect_error(
    quadkey_to_latlong(quadkeys),
    "Please, remove duplicated QuadKeys"
  )

  # Check for QuadKeys with different digit lengths
  expect_error(
    quadkey_to_latlong(c("2333", "123", "3310021")),
    "All the QuadKeys should have the same number of digits"
  )

  # Check if the returned object is an sf object
  result_sf <- quadkey_to_latlong(c("212", "213"))
  expect_true("sf" %in% class(result_sf),
    info = "Output should be of class 'sf'"
  )

  # Check if sf object contains expected columns
  expect_equal(colnames(result_sf), c("quadkey", "geometry"),
    info = "SF object should have 'quadkey' and 'geometry' columns"
  )

  # Check that `quadkey_to_latlong()` is giving correct values
  expect_equal(sf::st_coordinates(quadkey_to_latlong("0313102310"))[1], 
               -9.140625, tolerance = 1e-06)
  expect_equal(sf::st_coordinates(quadkey_to_latlong("0313102310"))[2], 
               53.33087, tolerance = 1e-06)
})

test_that("Error: QuadKey provided as string or in a character vectors", {
  # Test with a character vector
  expect_silent(quadkey_to_latlong(c('121', '222')))
    # Test with a numeric vector
  quadkeys2 <- c(101, 102, 103)
  expect_error(quadkey_to_latlong(quadkeys2),
               "Please provide QuadKeys a single string or a character vector")
})

test_that("Error: QuadKeys contain only the numbers '0', '1', '2', or '3'", {
  # Test with valid input
  valid_quadkey <- c("0123", "3333")
  expect_silent(quadkey_to_latlong(valid_quadkey))
  
  # Test with invalid input character
  invalid_quadkey <- c("123f", "3333")
  expect_error(quadkey_to_latlong(invalid_quadkey), 
               "QuadKeys can contain only the numbers '0', '1', '2', or '3'")
  # Test with invalid input number
  invalid_quadkey2 <- c("1234", "3333")
  expect_error(quadkey_to_latlong(invalid_quadkey2), 
               "QuadKeys can contain only the numbers '0', '1', '2', or '3'")
})
