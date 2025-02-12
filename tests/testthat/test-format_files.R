# Mock data for testing
data <- data.frame(
  quadkey = c(1e10, 2e10, 3e10, 1e10, 1e10), # scientific notation
  date_time = c(
    "2020-05-01 0800", "2020-05-01 0800", "2020-05-01 0800",
    "2020-05-01 0800", "2020-05-01 0800"
  ),
  n_crisis = c("10", "20", "\\N", "40", "\\N"),
  stringsAsFactors = FALSE
)

formatted_data <- format_fb_data(data)

# Test cases for `format_fb_data()`
test_that("`format_fb_data()` performs data formatting correctly", {
  # Check if scientific notation is removed in 'quadkey'
  expect_false(any(unlist(lapply(
    formatted_data$quadkey,
    function(x) any(grepl("e", as.character(x)))
  ))),
  info = "QuadKey column is not in scientific notation"
  )

  # Check if date format conversion is successful for 'day'
  expect_true(class(formatted_data$day) == "Date")

  # Check if time format conversion is successful for 'hour'
  expect_equal(formatted_data$hour, c(8, 8, 8, 8, 8))

  # Check if '\N' is replaced by NA except 'date_time' and 'day'
  expect_true(any(is.na(dplyr::select(formatted_data, -date_time, -day))))

  # Check if 'n_crisis' column is converted to numeric
  expect_true(class(formatted_data$n_crisis) == "numeric")
})

# Sample dataset
data <- data.frame(
  day = c("2023-01-01", "2023-01-03", "2023-01-05"),
  hour = c(0, 8, 16)
)

# Test cases for `missing_combinations()`
test_that("Test `missing_combinations()` function", {

  # Test if output is a data frame
  expect_s3_class(missing_combinations(data), "data.frame")

  # Test if output columns match expected columns 
  # if not other arguments are passed
  expect_true(all(c("day", "hour") %in% names(missing_combinations(data))))

  # Test if all missing combinations are identified
  expected_missing <- data.frame(
    day = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02", 
                    "2023-01-02", "2023-01-03", "2023-01-03", "2023-01-04", 
                    "2023-01-04", "2023-01-04", "2023-01-05", "2023-01-05")),
    hour = c(8, 16, 0, 8, 16, 0, 16, 0, 8, 16, 0, 8)
  )
  
  expect_equal(missing_combinations(data), expected_missing)
})
test_that("`read_fb_mobility_files()` passes keep_format arg correctly ", { 
  
  files <- read_fb_mobility_files(
    path_to_csvs = paste0(system.file("extdata",
                                      package = "quadkeyr"
    ), "/"), 
    keep_format = 'n_crisis',
    colnames = c( # The columns not listed here will be omitted
      "lat",
      "lon",
      "quadkey",
      "date_time",
      "n_crisis",
      "percent_change",
      "day",
      "hour"
    ),
    coltypes = list(
      lat = "d",
      lon = "d",
      quadkey = "c",
      date_time = "T",
      n_crisis = "c",
      percent_change = "c",
      day = "D",
      hour = "i"
    )
  )
expect_type(files$n_crisis, "character")
})

test_that("missing_combinations() works when all combinations are present", {
  data <- expand.grid(
    day = as.Date("2023-01-01"),
    hour = c(0, 8, 16)
  )
  
  result <- missing_combinations(data)
  expect_true(nrow(result) == 0)
})

# This are the column names we use for GeoCovidApp
test_that("missing_combinations() works with 'hora' and 'fecha' as col names", {
  data <- data.frame(
    fecha = as.Date(c("2023-01-01", "2023-01-02")),
    hora = c(0, 8)
  )
  
  expected <- data.frame(
    fecha = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02")),
    hora = c(8, 16, 0, 16)
  )
  
  result <- missing_combinations(data, hour_col = "hora", date_col = "fecha")
  expect_equal(result, expected)
})

test_that("missing_combinations() works with single-day input", {
  data <- data.frame(
    fecha = as.Date("2023-01-01"),
    hora = c(0, 8)
  )
  
  expected <- data.frame(
    fecha = as.Date("2023-01-01"),
    hora = c(16)
  )
  
  result <- missing_combinations(data, hour_col = "hora", date_col = "fecha")
  expect_equal(result, expected)
})



