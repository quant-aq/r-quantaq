#' Build and return URL
#'
#' Build and return the URL given an endpoint and any query parameters.
#'
#' @importFrom XML getRelativeURL
#'
#' @param endpoint A character vector of an endpoint. Can be a full or relative URL
#'
#' @param qs_params A list of query string parameters to format and attach to the URL (e.g. list(per_page = 10))
#'
#' @returns A character vector containing the final built URL.
#'
build_api_url <- function(endpoint, qs_params = NULL){
  client <- access_client()
  url_string <- paste(client$base_url, client$version, "", sep="/")
  combined_url_string <- XML::getRelativeURL(endpoint, url_string) # combined url string and path/endpoint

  final_url <- modify_url(combined_url_string, query = qs_params)

  return(final_url)
}

#' Complete an API request
#'
#' Generate an API request given endpoint, http verb, and any relevant query parameters.
#'
#' @importFrom httr GET
#'
#' @param endpoint A character string of the API endpoint to request.
#'
#' @param verb The httr function corresponding to the HTTP verb. Defaults to GET.
#'
#' @param qs_params Query string parameters.
#'
#' @examples
#' \dontrun{r <- request("account")}
#'
#' # include named arguments for query params, e.g. to limit the response to the first 5 results:
#' \dontrun{r <- request("devices/MOD-PM-00808/data", limit = 5)}
#'
#' @returns The parsed JSON from the API, including both data and metadata.
#'
request <- function(endpoint, verb = httr::GET, qs_params = NULL){
  client <- access_client()
  this_url <- build_api_url(endpoint, qs_params)

  resp <- verb(this_url,
            authenticate(client$api_key, ""),
            add_headers(user_agent = client$ua))

  # because API doesn't return json upon 400 and 500 errors, check that first
  if (http_error(resp)) {
    stop(
      sprintf(
        "[%s] -- QuantAQ API request failed \n%s\npath: <%s>",
        status_code(resp),
        content(resp, as = "text", encoding = "UTF-8"),
        this_url
      ),
      call. = FALSE
    )
  }

  # then, if we don't err out and it's JSON, parse it!
  if (http_type(resp) != "application/json"){
    stop("API did not return JSON", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)

  return(parsed)
}

#' Deal with paginated data.
#'
#' Iterates over and collated all pages of the data.
#'
#' @importFrom httr GET
#'
#' @param request Content from a \code{\link{request()}}
#'
#' @param verb The httr function corresponding to the HTTP verb. Defaults to GET.
#'
#' @returns All collated pages of the data.
#'
paginate <- function(response_content, verb = httr::GET){
  all_data <- response_content$data
  next_url <- response_content$meta$next_url

  #keep getting the next page while a new page exists, and append that page's data
  while(!is.null(next_url)){
    this_query <- parse_url(next_url)$query

    r <- do.call(request, list(next_url, qs_params = this_query))

    next_url <- r$meta$next_url

    all_data <- c(all_data, r$data) # append this page's data to all_data
  }

  return(all_data)
}

#' Handle requests params
#'
#' Format the parameters that can be passed to requests.
#'
#' @importFrom stringr str_split
#'
#' @param ... Parameters to be translated to query string and passed to the request.
#'
#' @returns A named list of query params.
#'
format_params <- function(...){
  # names(kwargs) <- lapply(names(kwargs),stringr::str_to_lower) # standardize param names
  kwargs <- list(...)
  filter <- stringr::str_split(kwargs$filter, ";")

  # TODO: set default per_page to 100?

  if("start" %in% names(kwargs)){
    filter <- c(filter,paste0("timestamp,ge,", kwargs$start))
    kwargs$start <- NULL
  }

  if("stop" %in% names(kwargs)){
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
#' @importFrom httr GET
#'
#' @param endpoint A character string of the API endpoint to request.
#'
#' @param verb The httr function corresponding to the HTTP verb. Defaults to GET.
#'
#' @param ... Params to be translated to query string.
#'
#' @returns Parsed data from the API.
#'
requests <- function(endpoint, verb = httr::GET, ...){
  kwargs <- format_params(...)
  r <- request(endpoint, verb = verb, qs_params = kwargs)

  this_data <- r

  pages <- r$meta
  if(!is.null(pages)){
    if(!is.null(pages$next_url) & pages$page != pages$pages){ # if next_url exists and we're not on the last page
      this_data <- paginate(r)
    } else {
      this_data <- r$data
    }
  }

  return(this_data)
}
