#' Setup the API client
#'
#' Sets up user details for querying the API
#'
#' Assigns API key, base url, version, and user agent to environment variables
#' for access by later API calls, and verifies these settings.
#'
#' @importFrom dplyr case_when
#'
#' @param api_type (Optional) A character string containing the API type -- 'prod' or 'dev' -- which determines the base url. Defaults to 'prod'.
#' @param api_key (Optional) A character string containing your QuantAQ API key. If left NULL, user will be prompted for it during execution.
#' @param version (Optional) A character string containing the API version you're querying. Defaults to 'v1'.
#' @param verbose (Optional) Default FALSE. Whether you want a message printed upon API connection.
#'
#' @returns No return value. Sets API details as environment variables.
#'
#' @export
setup_client <- function(api_type = 'prod', version = 'v1', api_key = NULL, verbose = FALSE) {
  ua <- user_agent("https://github.com/quant-aq/r-quantaq")

  base_url <- dplyr::case_when(
    api_type == 'prod' ~ 'https://api.quant-aq.com/device-api',
    api_type == 'dev' ~ 'https://dev.quant-aq.com/device-api'
  )

  if (is.null(api_key)){
    if (Sys.getenv("RSTUDIO") == "1"){ # if running from RStudio
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

  authenticate_client(verbose)
}

#' Access client variables
#'
#' @description
#' Retrieves the relevant client variables
#'
#' @details
#' A helper function that retrieves the API details set as environment variables
#' by [setup_client()].
access_client <- function(){
  client <- list(
    'api_key' = Sys.getenv('api_key'),
    'base_url' = Sys.getenv('base_url'),
    'version' = Sys.getenv('version'),
    'ua' = Sys.getenv('ua')
  )

  if(any(as.logical(lapply(client, function(x) x == "")))){
    stop("Some API details are missing! Did you remember to run `setup_client()`?")
  }

  return(client)
}

#' Authenticate API connection
#'
#' A helper function for [setup_client()] to determine that the user-provided
#' API details yields a proper connection with the API.
#'
#' @import httr
#' @param verbose (Optional) Default FALSE. Whether you want a message printed upon API connection.
#'
#' @returns No return value. `verbose = TRUE` causes a message to print to console upon successful API connection.
#'
authenticate_client <- function(verbose = FALSE){
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
    if(verbose == TRUE){
      cat("Successfully connected to QuantAQ API!")
    }
  }
}
