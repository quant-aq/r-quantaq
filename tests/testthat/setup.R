library(httr2)
library(lubridate)
setup_client(api_key=secret_decrypt("u7QHZzH2_KATioA_EjynwV8qi4JnDEI3raqjj6BMjS537hquEtsqcA", "QUANTAQ_PACKAGE_KEY"))
withr::defer(Sys.unsetenv(c('api_key', 'base_url', 'version', 'ua')), teardown_env())
