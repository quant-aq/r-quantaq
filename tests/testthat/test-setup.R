test_that("giving a bad api key throws a 401 error", {
  expect_error(setup_client(api_key = "fake_key"))
})
