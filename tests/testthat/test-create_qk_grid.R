test_that("Does the error message appears when inputs are incorrect?", {
  # Test case where xmin and xmax are identical (no area)
  expect_error(create_qk_grid(
    xmin = -50,
    xmax = -50,
    ymin = -34,
    ymax = -38,
    zoom = 6
  ),
  regexp = paste(
    "The selected inputs fail to generate a grid due to",
    "the limited area for this zoom level.",
    "Consider adjusting the zoom level or modifying",
    "the xmin, xmax, ymin, or ymax values."
  )
  )

  # Test case where ymin and ymax are identical (no area)
  expect_error(create_qk_grid(
    xmin = -59,
    xmax = -57,
    ymin = -34,
    ymax = -34,
    zoom = 6
  ),
  regexp = paste(
    "The selected inputs fail to generate a grid due to",
    "the limited area for this zoom level.",
    "Consider adjusting the zoom level or modifying",
    "the xmin, xmax, ymin, or ymax values."
  )
  )


  # Test case where all the values are identical (no area)
  expect_error(create_qk_grid(
    xmin = 0,
    xmax = 0,
    ymin = 0,
    ymax = 0,
    zoom = 6
  ),
  regexp = paste(
    "The selected inputs fail to generate a grid due to",
    "the limited area for this zoom level.",
    "Consider adjusting the zoom level or modifying",
    "the xmin, xmax, ymin, or ymax values."
  )
  )


  # Test case where the zoom level should be higher
  expect_error(create_qk_grid(
    xmin = -59,
    xmax = -58,
    ymin = -34,
    ymax = -35,
    zoom = 1
  ),
  regexp = paste(
    "The selected inputs fail to generate a grid due to",
    "the limited area for this zoom level.",
    "Consider adjusting the zoom level or modifying",
    "the xmin, xmax, ymin, or ymax values."
  )
  )

  # Test a case where the area is small but the zoom level is appropriate
  expect_silent(create_qk_grid(
    xmin = -59,
    xmax = -58,
    ymin = -34,
    ymax = -35,
    zoom = 12
  ))
})


# Check the zoom level

test_that("Check zoom level - Negative zoom", {
  expect_error(create_qk_grid(
    xmin = -59,
    xmax = -58,
    ymin = -34,
    ymax = -35,
    zoom = -12
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

test_that("Check zoom level - Greater than 23", {
  expect_error(create_qk_grid(
    xmin = -59,
    xmax = -58,
    ymin = -34,
    ymax = -35,
    zoom = 24
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

test_that("Check zoom level - Use of decimals", {
  expect_error(create_qk_grid(
    xmin = -59,
    xmax = -58,
    ymin = -34,
    ymax = -35,
    zoom = 12.3
  ),
  regexp = paste(
    "The zoom level should be an integer",
    "between 1 and 23"
  )
  )
})

# The area shouldn't be outside the possible map values
# (Microsoft Bing Tile System Documentation)

test_that("Check coordinate validation", {
  # Test case where ymin is below the minimum latitude
  expect_error(create_qk_grid(
    xmin = -59,
    xmax = -40,
    ymin = -86,
    ymax = -20,
    zoom = 6
  ),
  regexp = paste(
    "At least one of the provided coordinates",
    "are outside the valid range. Latitude must be",
    "between -85.05112878 and 85.05112878.",
    "Longitude must be between -180 and 180."
  )
  )

  # Test case where ymax is above the maximum latitude
  expect_error(create_qk_grid(
    xmin = -59,
    xmax = -40,
    ymin = -38,
    ymax = 86,
    zoom = 6
  ),
  regexp = paste(
    "At least one of the provided coordinates",
    "are outside the valid range. Latitude must be",
    "between -85.05112878 and 85.05112878.",
    "Longitude must be between -180 and 180."
  )
  )

  # Test case where xmin is below the minimum longitude
  expect_error(create_qk_grid(
    xmin = -181,
    xmax = -40,
    ymin = -38,
    ymax = -20,
    zoom = 6
  ),
  regexp = paste(
    "At least one of the provided coordinates",
    "are outside the valid range. Latitude must be",
    "between -85.05112878 and 85.05112878.",
    "Longitude must be between -180 and 180."
  )
  )

  # Test case where xmax is above the maximum longitude
  expect_error(create_qk_grid(
    xmin = -59,
    xmax = 181,
    ymin = -38,
    ymax = -20,
    zoom = 6
  ),
  regexp = paste(
    "At least one of the provided coordinates",
    "are outside the valid range. Latitude must be",
    "between -85.05112878 and 85.05112878.",
    "Longitude must be between -180 and 180."
  )
  )

  # Test case where all coordinates are within valid range
  expect_silent(create_qk_grid(
    xmin = -59,
    xmax = -40,
    ymin = -38,
    ymax = -20,
    zoom = 6
  ))
})
