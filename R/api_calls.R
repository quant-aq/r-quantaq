#' Build and return URL
#'
#' Build and return the URL given an endpoint and any query parameters.
#'
#' @importFrom XML getRelativeURL
#'
#' @param endpoint A character vector of an endpoint. Can be a full or relative URL
#'
#' @param query_params A list of query params to format and attach to the URL
#'
#' @returns A character vector containing the final built URL.
#'
build_api_url <- function(endpoint, query_params = NULL){
  client <- access_client()
  url_string <- paste(client$base_url, client$version, "", sep="/")
  combined_url_string <- XML::getRelativeURL(endpoint, url_string) # combined url string and path/endpoint

  final_url <- modify_url(combined_url_string, query = query_params)

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
#' @param query_params (Optional) A named list including any query arguments.
#'
#' @examples
#' \dontrun{r <- request("account")}
#' \dontrun{r <- request("devices/MOD-PM-00808/data", query_params = list(limit=5))}
#'
#' @returns A request object containing the response from the API.
#'
request <- function(endpoint, verb=httr::GET, query_params=NULL, filter_specs=NULL){
  client <- access_client()
  this_url <- build_api_url(endpoint, query_params)
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

  # structure(
  #   list(
  #     content = parsed,
  #     path = this_url,
  #     response = resp
  #   ),
  #   class = "request"
  # )

}

# print.request <- function(x, ...){
#   cat("<QuantAQ ", x$path, ">\n", sep="")
#   str(x$content)
#   invisible(x)
# }

#' Deal with paginated data.
#'
#' Iterates over and collated all pages of the data.
#'
#' @param request Content from a \code{\link{request()}}
#'
#' @returns The collated data from every page of the data.
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

# requests <- function(endpoint, verb=httr:GET, query_params = NULL){
#
# }


# TODO:
# data_to_dataframe <- function(){}
# TODO: make every specific get request return am object, and then provide a "as.data.frame" for each of them?

get_teams <- function(id = NULL){
  request("teams")
}

whoami <- function(){
  request("account")
}

get_devices <- function(sn = NULL){
  endpoint <- paste("devices", sn, sep = "/")
  request(endpoint)
}

get_device_metadata <- function(sn = NULL){
  endpoint <- paste("meta-data", sn, sep="/")
  request(endpoint)
}

get_data <- function(sn, start = NULL, stop = NULL){
  endpoint <- paste("devices", sn, "data", sep = "/")
  request(endpoint, query_params = list("start" = start, "stop" = stop))
}

