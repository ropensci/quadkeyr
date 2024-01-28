test_that("Check if column 'var_n_percent_7' exists", {

  output <- result_read_fb_mobility_data[1:100,] |> apply_weekly_lag()

  # Check if 'percent_change_7' exists in the output columns
  expect_true("percent_change_7" %in% colnames(output),
              info = "Column 'percent_change_7' should exist in the output")

  expect_equal(nrow(output), 
               sum(!is.na(result_read_fb_mobility_data[1:100,]$n_crisis)),
               info = "The output is analysing the exact number of quadkeys")
})

# Note: there is not possible to test if the lag7 is working