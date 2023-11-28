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
  ua <- "https://github.com/quant-aq/r-quantaq"

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
#' @param verbose (Optional) Default FALSE. Whether you want a message printed upon API connection.
#'
#' @returns No return value. `verbose = TRUE` causes a message to print to console upon successful API connection.
#'
authenticate_client <- function(verbose = FALSE){
  client <- access_client()

  withCallingHandlers(
    quantaq_request('account'),
    httr2_http_401 = function(cnd) {
      rlang::abort("Invalid API Key!", parent = cnd)
    },
    httr2_http_404 = function(cnd) {
      rlang::abort("Endpoint not found!", parent = cnd)
    }
  )

  if(verbose == TRUE){
    cat("Successfully connected to QuantAQ API!")
  }
}
