test_that("giving a bad api key throws a 401 error", {
  expect_error(setup_client(api_key = "fake_key"))
  # need to reset this, because otherwise test environment remains polluted by bad key
  setup_client(api_key=secret_decrypt("u7QHZzH2_KATioA_EjynwV8qi4JnDEI3raqjj6BMjS537hquEtsqcA", "QUANTAQ_PACKAGE_KEY"))
})
