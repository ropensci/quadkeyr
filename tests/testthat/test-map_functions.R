test_that("mapsize can reproduce values reported in the Documentation", {
  expect_equal(
    mapsize(level = 6),
    16384
  )
})

test_that("clip function logic works", {
  expect_equal(
    clip(
      n = 1,
      min_value = 3,
      max_value = 5
    ),
    3
  )
  expect_equal(
    clip(
      n = 4,
      min_value = 3,
      max_value = 5
    ),
    4
  )
  expect_equal(
    clip(
      n = 188,
      min_value = 3,
      max_value = 5
    ),
    5
  )
})

test_that("ground_res can reproduce values reported in the Documentation", {
  expect_equal(
    round(ground_res(
      latitude = 0,
      level = 6
    ),
    digits = 3
    ), # there are minor issues with decimals
    2445.985
  )
})

test_that("mapscale can reproduce values reported in the Documentation", {
  expect_equal(mapscale(
    latitude = 0,
    level = 6,
    screen_dpi = 96
  ),
  9244667,
  tolerance = 1e-3
  ) # there are minor issues with decimals
})
