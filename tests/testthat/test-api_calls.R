test_that("build_api_url deals with full URL", {
  api_url <- build_api_url("https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")

  expect_identical(api_url, "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")
})

test_that("build_api_url deals with api partial endpoint", {
  api_url <- build_api_url("devices/MOD-PM-00808/data/?page=2&per_page=50")

  expect_identical(api_url, "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=2&per_page=50")
})

test_that("build_api_url deals with added query params in query_params arg", {
  built_url <- build_api_url("https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=1", query_params = list(per_page = 50))

  expected_url <- "https://api.quant-aq.com/device-api/v1/devices/MOD-PM-00808/data/?page=1&per_page=50"

  expect_identical(built_url, expected_url)
})

test_that("request GET handles trailing slash", {
  r1 <- request("devices")
  r2 <- request("devices/")

  expect_identical(r1,r2)
})

test_that("paginate returns proper size data when given multiple pages", {
  r <- request("devices/MOD-PM-00808/data/", query_params = list(per_page = 50, limit = 200))

  paginated_data <- paginate(r)

  expect_equal(length(paginated_data), 200)
})

test_that("paginate returns proper size when given one page", {
  r <- request("devices/MOD-PM-00808/data/", query_params = list(limit = 10))

  paginated_data <- paginate(r)

  expect_equal(length(paginated_data), 10)
})

