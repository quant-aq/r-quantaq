#' Build and return URL
#'
#' Build and return the URL given an endpoint and any query parameters.
#'
#' @importFrom XML getRelativeURL
#'
#' @param endpoint A character vector of an endpoint. Can be a full or relative URL
#'
#' @param ... Query parameters to format and attach to the URL (e.g. per_page)
#'
#' @returns A character vector containing the final built URL.
#'
build_api_url <- function(endpoint, ...){
  client <- access_client()
  url_string <- paste(client$base_url, client$version, "", sep="/")
  combined_url_string <- XML::getRelativeURL(endpoint, url_string) # combined url string and path/endpoint

  final_url <- modify_url(combined_url_string, query = list(...))

  return(final_url)
}

#' Complete an API request
#'
#' Generate an API request given endpoint, http verb, and any relevant query parameters.
#'
#' @import httr
#'
#' @param endpoint A character string of the API endpoint to request.
#'
#' @param verb The httr function representing the HTTP verb. Defaults to GET.
#'
#' @param ... Named query parameters to
#'
#' @examples
#' \dontrun{r <- request("account")}
#'
#' # include named arguments for query params, e.g. to limit the response to the first 5 results:
#' \dontrun{r <- request("devices/MOD-PM-00808/data", limit = 5)}
#'
#' @returns The parsed JSON from the API, including both data and metadata.
#'
request <- function(endpoint, verb=httr::GET, ...){
  # query_params = list(...)
  client <- access_client()
  this_url <- build_api_url(endpoint, ...)

  resp <- verb(this_url,
              authenticate(client$api_key, ""),
              add_headers(user_agent = client$ua))

  # because API doesn't return json upon 400 and 500 errors, check that first
  if (http_error(resp)) {
    stop(
      sprintf(
        "[%s] -- QuantAQ API request failed \n%s\npath: <%s>",
        status_code(resp),
        content(resp, "text"),
        this_url
      ),
      call. = FALSE
    )
  }

  # then, if we don't err out and it's JSON, parse it!
  if (http_type(resp) != "application/json"){
    stop("API did not return JSON", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  return(parsed)
}

#' Deal with paginated data.
#'
#' Iterates over and collated all pages of the data.
#'
#' @param request Content from a \code{\link{request()}}
#'
#' @returns All collated pages of the data.
paginate <- function(response_content){
  all_data <- response_content$data
  next_url <- response_content$meta$next_url

  #keep getting the next page while a new page exists, and append that page's data
  while(!is.null(next_url)){
    this_query <- parse_url(next_url)$query

    r <- request(next_url, query_params = this_query)

    next_url <- r$meta$next_url

    all_data <- c(all_data, r$data) # append this page's data to all_data
  }

  return(all_data)
}

#' Request that handles pagination
#'
#' @returns Parsed data from the API.
#'
#'
requests <- function(endpoint, verb=httr:GET, query_params = NULL){
  if(is.null(query_params)){
    names(query_params) <- lapply(names(query_params),stringr::str_to_lower) # standardize params

    # TODO: how to handle unrecognized args/params to API? does API throw error?

  }


}

