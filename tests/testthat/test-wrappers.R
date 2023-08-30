test_that("get devices passes limit properly",{
  this_limit = 12

  expect_equal(this_limit, length(get_devices(limit = this_limit)))
})

test_that("get data passes limit properly",{
  this_limit = 10

  expect_equal(this_limit, length(get_data("MOD-PM-00808", limit = this_limit)))
})

test_that("get_data_by_date returns data with timestamps within the appropriate bounds", {
  tzone <- Sys.timezone()
  yesterday <- lubridate::today(tzone = tzone) - 1
  data <- get_data_by_date("MOD-PM-00808", yesterday) %>% as.data.frame

  expected_interval <- lubridate::interval(yesterday, lubridate::today(tzone = tzone))

  timestamps_are_within_interval <- as.logical(lapply(data$timestamp, function(ts) lubridate::`%within%`(ts, expected_interval)))

  expect_true(all(timestamps_are_within_interval))
})
