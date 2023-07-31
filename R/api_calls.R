# params/queries to include:
# limit (e.g.: return first 5 devices)
# sort (with 'by' for choosing attribute)
# filter
# per_page
# for data: allow raw vs final data
# start and stop datetimes
#
# paginate

request <- function(endpoint, verb=httr::GET, query_params=NULL){
  client <- access_client()
  path <-  paste0(client$version, '/', endpoint)
  url <- modify_url(client$base_url, path=path, query = query_params)
  resp <- verb(url,
              authenticate(client$api_key, ""),
              add_headers(user_agent = client$ua))

  # because API doesn't return json upon 400 and 500 errors, check that first
  if (http_error(resp)) {
    stop(
      sprintf(
        "[%s] -- QuantAQ API request failed \n%s\npath: <%s>",
        status_code(resp),
        content(resp, "text"),
        path
      ),
      call. = FALSE
    )
  }

  # then, if we don't err out and it's JSON, parse it!
  if (http_type(resp) != "application/json"){
    stop("API did not return JSON", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "request"
  )

}

print.request <- function(x, ...){
  cat("<QuantAQ ", x$path, ">\n", sep="")
  str(x$content)
  invisible(x)
}

# get all the data from a paginated request
paginate <- function(endpoint){
  r <- request(endpoint)
  all_data <- r$content$data

  #keep getting the next page while a new page exists
  while(!is.null(r$data$meta$next_url)){
    all_data <- c(all_data, r$data)
  }
}

get_teams <- function(id = NULL){
  request("teams")
}

whoami <- function(){
  request("account")
}

get_devices <- function(sn = NULL){
  endpoint <- paste("devices", sn, sep = "/") %>%
    gsub('/$', '', .) #remove trailing slash in case no sn was provided

  request(endpoint)
}

get_device_metadata(sn = NULL){
  endpoint <- paste("meta-data", sn, sep="/") %>%
    gsub("/$", "", .) #remove trailing slash in case no sn was provided

  request(endpoint)
}

get_data <- function(sn, start = NULL, stop = NULL){
  endpoint <- paste("devices", sn, "data", sep = "/")

  request(endpoint, query_params = list("start" = start, "stop" = stop))
}

