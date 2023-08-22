setup_client(api_key = "9NY39W05Z7R9J0OFQ1S4LVCV")
withr::defer(Sys.unsetenv(c('api_key', 'base_url', 'version', 'ua')), teardown_env())
