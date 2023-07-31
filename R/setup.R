library(httr)

#' Setup the api client
#'
#' @description
#' `setup_client` sets up the api key and url for querying the api
#'
#' @details
#' Requests the api key, creates the url and user agent, and assigns
#' them all to environment variables for access by later api calls.
#' The url is created by combining the provided 'version' number string with the
#' appropriate base url (determined according to the provided 'api_type').
#'
#' @param api_type character The api type -- 'prod' or 'dev'. Defaults to 'prod'.
#'
#' @param api_key character Your QuantAQ API key.
#'
#' @param version character The API version you're querying. Defaults to 'v1'.
#'
#'@export
setup_client <- function(api_type = 'prod', api_key = NULL, version = 'v1') {
  ua <- user_agent("https://github.com/quant-aq/r-quantaq")

  base_url <- dplyr::case_when(
    api_type == 'prod' ~ 'https://api.quant-aq.com/device-api',
    api_type == 'dev' ~ 'https://dev.quant-aq.com/device-api'
  )

  if (is.null(api_key)){
    if (Sys.getenv("RSTUDIO") == "1"){
      api_key <- rstudioapi::askForSecret(
        name = "QuantAQ API",
        message = 'Enter your API Key',
        title = "QuantAQ API Key")
    } else {
      api_key <- readline(prompt = "Please enter your QuantAQ API Key:")
    }
  }

  Sys.setenv(
    'api_key' = api_key,
    'base_url' = base_url,
    'version' = version,
    'ua' = ua
    )

  authenticate_client()
}

#' Access client variables
#'
#' @description
#' `access_client` retrieves the relevant client variables
#'
#' @details
#' A helper function that retrieves the environment variables set by setup_client,
#' used for interfacing with the api. This is the api key, base url, version,
#' and user agent.
#'
#'@export
access_client <- function(){
  return(list(
    'api_key' = Sys.getenv('api_key'),
    'base_url' = Sys.getenv('base_url'),
    'version' = Sys.getenv('version'),
    'ua' = Sys.getenv('ua')
  ))
}

#' Authenticate api connection
#'
#' @description
#'`authenticate_client` confirms API authentication
#'
#' @details
#' A helper function for `client_setup()` to determines that the provided API
#' key provides a proper connection with the API.
#'
authenticate_client <- function(){
  client <- access_client()
  path <- paste0(client$version,'/account') # just use "account" endpoint to authenticate
  url <- modify_url(client$base_url, path=path)
  code <- status_code(GET(
    url,
    authenticate(client$api_key, ""),
    add_headers(user_agent = client$ua)
  ))
  if (code == 401){
    stop(paste0(http_status(code)$message, " -- Invalid API Key!"))
  } else if (code != 200){
    stop(http_status(code)$message)
  } else {
    cat("Successfully connected to QuantAQ API!")
  }
}
