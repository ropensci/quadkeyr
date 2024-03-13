# Set seed for reproducibility
set.seed(123)

# Test data frame
data <- expand.grid(
  quadkey = c(
    "21032302022230",
    "21032013230023",
    "21032213321302",
    "21032130110130",
    "21032123021123",
    "21032102213221"
  ),
  day = seq(as.Date("1996-05-10"), by = "day", length.out = 10),
  hour = c(0, 8, 16)
) |>
  dplyr::arrange(quadkey, day, hour)

data <-  data |>
  dplyr::mutate(n_crisis = sample(1:5,
                                  nrow(data),
                                  replace = TRUE))

# Insert NAs into specific positions
positions_to_insert_na <- sample(seq_len(nrow(data)), 50, replace = TRUE)
data$n_crisis[positions_to_insert_na] <- NA

test_that("`apply_weekly_lag()` produces a 7 day lag in `n_crisis`", {
  output <- apply_weekly_lag(data = data)
  
  data_test <- output |>
    dplyr::mutate(n = dplyr::lag(paste(quadkey,
                                day,
                                hour),
                          n = 7 * 3))   |>
    tidyr::separate(
      col = n,
      into = c('qk_test',
               'day_test',
               'hour_test'),
      sep = " "
    )
  
  # test that the lag is correct
  value <- subset(data_test,
                  quadkey == '21032102213221' &
                    day == '1996-05-11' &
                    hour == 8)
  
  value_test <- subset(data_test,
                       qk_test == '21032102213221' &
                         day_test == '1996-05-11' &
                         hour_test == 8)
  
  expect_equal(value$n_crisis, value_test$n_crisis_lag_7)
  
  # check if the amount of NAs is correct for the lag created
  values_NA <- subset(data_test,
                      quadkey == '21032123021123' &
                        day %in% seq(as.Date("1996-05-10"),
                                     as.Date("1996-05-16"), by = "day"))
  sum_NA_7 <- sum(is.na(values_NA$n_crisis_lag_7))
  sum_NA_perc_7 <- sum(is.na(values_NA$percent_change_7))
  
  expect_equal(sum_NA_7, 21) # 7 day lag * 3 hours
  expect_equal(sum_NA_perc_7, 21)
})


