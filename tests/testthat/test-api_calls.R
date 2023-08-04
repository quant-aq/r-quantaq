#-------------- build_api_url()

test_that("build_api_url deals with full URL", {
  api_url <- build_api_url("https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")

  expect_identical(api_url, "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")
})

test_that("build_api_url deals with api partial endpoint", {
  api_url <- build_api_url("devices/MOD-PM-00808/data/?page=2&per_page=50")

  expect_identical(api_url, "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")
})

test_that("build_api_url deals with added query params in query_params arg", {
  built_url <- build_api_url("https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=1", per_page = 50)

  expected_url <- "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=1&per_page=50"

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
  content <- request("devices", limit = this_limit)
  expect_equal(length(content$data), this_limit)
  expect_equal(content$meta$total, this_limit)
})

#-------------- paginate()

test_that("paginate returns proper size data when given multiple pages", {
  r <- request("devices/MOD-PM-00808/data/", per_page = 50, limit = 200)

  paginated_data <- paginate(r)

  expect_equal(length(paginated_data), 200)
})

test_that("paginate returns proper size when given one page", {
  r <- request("devices/MOD-PM-00808/data/", limit = 10)

  paginated_data <- paginate(r)

  expect_equal(length(paginated_data), 10)
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
  # both the following calls should yield the same result because the fx always does "start" first, then "stop"
  p1 <- format_params(limit = 500, start = "2020-01-01 00:00", stop = "2023-03-03 00:00", filter = "device_state,eq,ACTIVE")
  p2 <- format_params(limit = 500, stop = "2023-03-03 00:00", start = "2020-01-01 00:00", filter = "device_state,eq,ACTIVE")

  expected_string <- "device_state,eq,ACTIVE;timestamp,ge,2020-01-01 00:00;timestamp,le,2023-03-03 00:00"

  expect_identical(p1$filter, expected_string)
  expect_identical(p2$filter, expected_string)
})


#-------------- requests()

# test_that("requests with start/stop returns data from the expected timestamp ranges")
# request("devices/MOD-PM-00808/data/", start = "2023-08-03T20:33:42")
