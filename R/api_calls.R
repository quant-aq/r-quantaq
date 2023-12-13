#' Complete an API request
#'
#' Generate an API request given an endpoint and any relevant query parameters.
#'
#' @param endpoint A character string of the API endpoint to request. Can be a full url, or a relative endpoint for the QuantAQ API
#' @param qs_params Query string parameters.
#'
#' @examples
#' \dontrun{r <- quantaq_request("account")}
#'
#' # include a named list for query params, e.g. to limit the response to the first 5 results:
#' \dontrun{r <- quantaq_request("devices/MOD-PM-00808/data", list(limit = 5))}
#'
#' @returns The parsed JSON from the API, including both data and metadata.
#'
quantaq_request <- function(endpoint, qs_params = NULL){
  client <- access_client()

  is_full_url <- stringr::str_detect(endpoint, "^http")

  if(is_full_url){ # if it's a full url
    req <- httr2::request(endpoint)
  } else{ # otherwise, build the url from the relative path
    req <- httr2::request(client$base_url) %>%
      httr2::req_url_path_append(client$version) %>%
      httr2::req_url_path_append(endpoint)
  }

  req %>% httr2::req_url_query(!!!qs_params) %>%
    httr2::req_auth_basic(client$api_key, "") %>%
    httr2::req_user_agent(client$ua) %>%
    httr2::req_perform()
}

#' Deal with paginated data.
#'
#' Iterates over and collated all pages of the data.
#'
#' @param response_content JSON content from a [quantaq_request()] response
#'
#' @returns All collated pages of the data.
#'
paginate <- function(response_content){
  all_data <- response_content$data
  next_url <- response_content$meta$next_url

  #keep getting the next page while a new page exists, and append every new page's data
  while(!is.null(next_url)){
    r <- quantaq_request(next_url) %>% httr2::resp_body_json()

    next_url <- r$meta$next_url

    all_data <- c(all_data, r$data) # append this page's data to all_data
  }

  return(all_data)
}

#' Handle requests params
#'
#' A helper function to format the parameters that can be passed to [requests()].
#'
#' @importFrom stringr str_split
#' @param ... Parameters to be translated to query string and passed to the request.
#' @returns A named list of query params.
#'
format_params <- function(...){
  kwargs <- list(...)
  filter <- stringr::str_split(kwargs$filter, ";", simplify = TRUE)
  filter <- filter[filter != ""] # drop empty string from filter

  # TODO: set default per_page to 100?
  # TODO: give some warning when passed params are not recognized?

  if("start" %in% names(kwargs) && !is.null(kwargs$start)){
    filter <- c(filter,paste0("timestamp,ge,", kwargs$start))
    kwargs$start <- NULL
  }

  if("stop" %in% names(kwargs) && !is.null(kwargs$stop)){
    filter <- c(filter,paste0("timestamp,le,", kwargs$stop))
    kwargs$stop <- NULL
  }

  # format the filter string. If it's not empty, add it to kwargs
  filter <- paste(filter, collapse = ";")

  if(!filter == ""){
    kwargs$filter <- filter
  }

  return(kwargs)
}

#' Request that handles pagination
#'
#' @importFrom httr2 resp_body_json
#' @param endpoint A character string of the API endpoint to request.
#' @param ... Params to be translated to query string.
#'
#' @returns Parsed data from the API.
#'
requests <- function(endpoint, ...){
  kwargs <- format_params(...)
  r <- quantaq_request(endpoint, qs_params = kwargs) %>% httr2::resp_body_json()

  this_data <- r

  meta <- r$meta
  if(!is.null(meta)){ # if meta exists
    if(!is.null(meta$date)){ #if we're dealing with data-by-date
      this_data <- r$data
    }
    else if(!is.null(meta$next_url) & meta$page != meta$pages){ # if we're dealing with paginated data (i.e. next_url exists), and we're not on the last page
      this_data <- paginate(r)
    } else {
      this_data <- r$data
    }
  }

  return(this_data)
}
