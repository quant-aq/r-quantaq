#-------------- build_api_url()

test_that("build_api_url deals with full URL", {
  api_url <- build_api_url("https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")

  expect_identical(api_url, "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")
})

test_that("build_api_url deals with api partial endpoint", {
  api_url <- build_api_url("devices/MOD-PM-00808/data/?page=2&per_page=50")

  expect_identical(api_url, "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")
})

test_that("build_api_url deals with added keyword args", {
  built_url <- build_api_url("https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=1", qs_params = list(per_page = 50))

  expected_url <- "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=1&per_page=50"

  expect_identical(built_url, expected_url)
})

test_that("build_api_url drops any NULL qs_params", {
  built_url <- build_api_url("devices", qs_params = list(limit = 10, sort = NULL, hop = NULL))

  expected_url <- "https://api.quant-aq.com/device-api/v1/devices?limit=10"

  expect_identical(built_url, expected_url)
})

#-------------- request()

test_that("request GET handles trailing slash", {
  r1 <- request("devices", verb = httr::GET)
  r2 <- request("devices/", verb = httr::GET)

  expect_identical(r1,r2)
})

test_that("request GET with limit returns appropriate amount", {
  this_limit <- 2
  content <- request("devices", qs_params = list(limit = this_limit))
  expect_equal(length(content$data), this_limit)
  expect_equal(content$meta$total, this_limit)
})

test_that("request GET with timestamp filter returns appropriate timestamps", {
  earliest_string <- "2023-08-06T22:00:00"
  latest_string <- "2023-08-06T22:06:00"

  r <- request("devices/MOD-PM-00808/data/",
               qs_params = list(filter = paste0("timestamp,ge,", earliest_string, ";timestamp,le,", latest_string)))

  first_sample_timestamp <- lubridate::ymd_hms(r$data[[1]]$timestamp)
  last_sample_timestamp <- lubridate::ymd_hms(r$data[[length(r$data)]]$timestamp)
  expected_interval <- lubridate::interval(lubridate::ymd_hms(earliest_string), lubridate::ymd_hms(latest_string))

  expect_true(lubridate::`%within%`(first_sample_timestamp,expected_interval))
  expect_true(lubridate::`%within%`(last_sample_timestamp,expected_interval))
})

test_that("request GET with sort option returns appropriately sorted timestamps",{
  r <- request("devices/MOD-PM-00808/data/",
               qs_params = list(sort = "timestamp,asc", limit = 10))

  first_sample_timestamp <- lubridate::ymd_hms(r$data[[1]]$timestamp)
  second_sample_timestamp <- lubridate::ymd_hms(r$data[[2]]$timestamp)
  penultimate_sample_timestamp <- lubridate::ymd_hms(r$data[[length(r$data) - 1]]$timestamp)
  last_sample_timestamp <- lubridate::ymd_hms(r$data[[length(r$data)]]$timestamp)

  expect_true(first_sample_timestamp <= second_sample_timestamp)
  expect_true(first_sample_timestamp <= last_sample_timestamp)

  expect_true(second_sample_timestamp <= penultimate_sample_timestamp)
  expect_true(second_sample_timestamp <= last_sample_timestamp)

  expect_true(penultimate_sample_timestamp <= last_sample_timestamp)

  expect_false(is.unsorted(c(first_sample_timestamp, second_sample_timestamp, penultimate_sample_timestamp, last_sample_timestamp)))
  expect_true(is.unsorted(c(second_sample_timestamp, first_sample_timestamp, last_sample_timestamp, penultimate_sample_timestamp)))
})

#-------------- paginate()

test_that("paginate returns proper size data when given multiple pages", {
  r <- request("devices/MOD-PM-00808/data/", qs_params = list(per_page = 50, limit = 200))

  paginated_data <- paginate(r)

  expect_equal(length(paginated_data), 200)
})

test_that("paginate returns proper size when given one page", {
  r <- request("devices/MOD-PM-00808/data/", qs_params = list(limit = 10))

  paginated_data <- paginate(r)

  expect_equal(length(paginated_data), 10)
})

test_that("all the data in a paginate call with a timestamp filtered request gives appropriate timestamps", {
  earliest_string <- "2023-08-06T20:00:00"
  latest_string <- "2023-08-06T22:06:00"

  r <- request("devices/MOD-PM-00808/data/",
               qs_params = list(filter = paste0("timestamp,ge,", earliest_string, ";timestamp,le,", latest_string)))

  all_data <- paginate(r)

  first_sample_timestamp <- lubridate::ymd_hms(all_data[[length(all_data)]]$timestamp)
  last_sample_timestamp <- lubridate::ymd_hms(all_data[[1]]$timestamp)
  expected_interval <- lubridate::interval(lubridate::ymd_hms(earliest_string), lubridate::ymd_hms(latest_string))

  expect_true(lubridate::`%within%`(first_sample_timestamp, expected_interval))
  expect_true(lubridate::`%within%`(last_sample_timestamp, expected_interval))
})

#-------------- format_params()

test_that("format_params properly attaches named query params", {
  expect_equal(format_params(limit = 500)$limit, 500)
})

test_that("format_params properly attaches start and stop", {
  params <- format_params(start = "2020-01-01 00:00")

  expect_identical(params$filter, "timestamp,ge,2020-01-01 00:00")
})

test_that("format_params with filter already existing properly attaches start and stop to that string", {
  params <- format_params(start = "2020-01-01 00:00", filter = "device_state,eq,ACTIVE")

  expect_identical(params$filter, "device_state,eq,ACTIVE;timestamp,ge,2020-01-01 00:00")
})

test_that("format_params with filter and other named query params attaches everything correctly", {
  params <- format_params(limit = 500, start = "2020-01-01 00:00", filter = "device_state,eq,ACTIVE", sort = "timestamp,asc")
  expect_identical(params$filter, "device_state,eq,ACTIVE;timestamp,ge,2020-01-01 00:00")
  expect_equal(params$limit, 500)
  expect_identical(params$sort, "timestamp,asc")
})

test_that("format_params with both start and stop formats correctly", {
  # both the following calls should yield the same result because the fx always handles "start" first, then "stop"
  p1 <- format_params(limit = 500, start = "2020-01-01 00:00", stop = "2023-03-03 00:00", filter = "device_state,eq,ACTIVE")
  p2 <- format_params(limit = 500, stop = "2023-03-03 00:00", start = "2020-01-01 00:00", filter = "device_state,eq,ACTIVE")

  expected_string <- "device_state,eq,ACTIVE;timestamp,ge,2020-01-01 00:00;timestamp,le,2023-03-03 00:00"

  expect_identical(p1$filter, expected_string)
  expect_identical(p2$filter, expected_string)
})

test_that("format params properly drops start and stop when NULL", {
  out_kwargs <- format_params(start = NULL, stop = NULL)

  # there should not be a "filter" kwarg in the return from format_params
  expect_false("filter" %in% names(out_kwargs))
})

#-------------- requests()

test_that("requests with multiple pages returns more data than a simple request to the same endpoint", {
  this_limit <- 100
  paginated_data <- requests("devices/MOD-PM-00808/data/", limit = this_limit)
  single_request_data <- request("devices/MOD-PM-00808/data/?limit=100")$data

  expect_true(length(paginated_data) > length(single_request_data))

})

test_that("requests handles data-by-date",{
  this_date <- "2023-08-22"
  x <- requests(paste("devices/MOD-PM-00808/data-by-date", this_date, sep = "/"))

  all_within <- as.logical(lapply(x, function(y) parse_date_time(y$timestamp, "Ymd H:M:S.") %within% as.interval(1, ymd(this_date))))

  expect_true(all(all_within))
})

test_that("requests for something other than data with only one page returns as expected", {
  x <- requests("teams")

  expect_true(is.list(x))
})

test_that("requests for data with only one page returns as expected", {
  this_limit <- 10

  x <- requests("devices/MOD-PM-00808/data/", limit = this_limit)

  expect_equal(length(x), this_limit)
})

test_that("data requests that return only one datapoint are same structure as things with multiple elements",{
  x <- requests("devices/MOD-PM-00808/data/", limit = 1)
  y <- requests("devices/MOD-PM-00808/data/", limit = 25)

  expect_identical(class(x), class(y))
})

test_that("requests with start/stop returns data from the expected timestamp ranges", {
  earliest_string <- "2023-08-06 22:00:00"
  latest_string <- "2023-08-06 22:06:00"

  x <- requests("devices/MOD-PM-00808/data/", start = earliest_string, stop = latest_string)

  first_sample_timestamp <- lubridate::ymd_hms(x[[length(x)]]$timestamp)
  last_sample_timestamp <- lubridate::ymd_hms(x[[1]]$timestamp)
  expected_interval <- lubridate::interval(lubridate::ymd_hms(earliest_string), lubridate::ymd_hms(latest_string))

  expect_true(lubridate::`%within%`(first_sample_timestamp,expected_interval))
  expect_true(lubridate::`%within%`(last_sample_timestamp,expected_interval))
})
