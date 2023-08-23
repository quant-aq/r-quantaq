test_that("get devices passes limit properly",{
  this_limit = 12

  expect_equal(this_limit, length(get_devices(limit = this_limit)))
})

test_that("get data passes limit properly",{
  this_limit = 10

  expect_equal(this_limit, length(get_data("MOD-PM-00808", limit = this_limit)))
})


# test_that("get_devices appropriately errs out if not given named arguments", {
#   expect_failure(get_devices("MOD-PM-00808", 10), "get_devices")
# })

test_that("get_data properly handles data-by-date", {

})


test_that("get_data properly handles data-by-date and raw together and separately", {

})


