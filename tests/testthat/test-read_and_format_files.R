# Mock data for testing
data <- data.frame(
  quadkey = c(1e10, 2e10, 3e10, 1e10, 1e10),
  date_time = c("2020-05-01 0800", "2020-05-01 0800", "2020-05-01 0800",
                "2020-05-01 0800", "2020-05-01 0800"),
  n_crisis = c("10", "20", "\\N", "40", "\\N"),
  stringsAsFactors = FALSE
)

formatted_data <- format_data(data)

# Test cases for format_data function
test_that("format_data function performs data formatting correctly", {
  # Test 1: Check if scientific notation is removed in 'quadkey'

  expect_false(any(sapply(formatted_data$quadkey, function(x) any(grepl("e", as.character(x))))),
               info = "QuadKey column is not in scientific notation")

  # Test 2: Check if date format conversion is successful for 'day'
  expect_true(class(formatted_data$day) == "Date",
              info = "Date format should be converted in 'day'")

  # Test 3: Check if time format conversion is successful for 'time'
  expect_equal(formatted_data$time, c(8, 8, 8, 8, 8),
               info = "Time format should be converted to numeric 'time'")

  # Test 4: Check if '\N' is replaced by NA in columns except 'date_time' and 'day'
  expect_true(any(is.na(dplyr::select(formatted_data, -date_time, -day))),
               info = "The '\\N' values should be replaced by NA")

  # Test 5: Check if 'n_crisis' column is converted to numeric
  expect_true(class(formatted_data$n_crisis) == "numeric",
              info = "'n_crisis' column should be converted to numeric")
})


# Sample dataset
data <- data.frame(
  day = c("2023-01-01", "2023-01-03", "2023-01-05"),
  time = c(0, 8, 16)
)

# Test cases for missing_combinations function
test_that("Test missing_combinations function", {

  # Test if output is a data frame
  expect_is(missing_combinations(data), "data.frame")

  # Test if output columns match expected columns
  expect_true(all(c("day", "time") %in% names(missing_combinations(data))))

  # Test if all missing combinations are identified
  expected_missing <- structure(list(day = structure(c(19359, 19360, 19361, 19362,
                                                       19358, 19359, 19361, 19362,
                                                       19358, 19359, 19360, 19361),
                                                     class = "Date"),
                                     time = c(0, 0, 0, 0, 8, 8, 8, 8, 16, 16, 16, 16)),
                                out.attrs = list(
                                       dim = c(day = 5L, time = 3L),
                                       dimnames = list(day = c("day=2023-01-01",
                                                               "day=2023-01-02",
                                                               "day=2023-01-03",
                                                               "day=2023-01-04",
                                                               "day=2023-01-05"
                                       ), time = c("time= 0", "time= 8", "time=16"))),
                                class = "data.frame", row.names = c(NA, -12L))

  expect_equal(missing_combinations(data), expected_missing)

})
