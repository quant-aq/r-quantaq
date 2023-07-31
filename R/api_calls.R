
api_request <- function(endpoint, verb=httr::GET){
  client <- access_client()
  path <-  paste0(client$version,'/',endpoint)
  url <- modify_url(client$base_url, path=path)
  resp <- verb(url,
              authenticate(client$api_key, ""),
              add_headers(user_agent=client$ua))

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
    class = "api_request"
  )

}


# params/queries to include:
# limit (e.g.: return first 5 devices)
# sort (with 'by' for choosing attribute)
# filter
# per_page
# allow raw vs final data
# start and stop datetimes
api_get <- function(endpoint){
  client <- access_client()
  path <-  paste0(client$version,'/',endpoint)
  url <- modify_url(client$base_url, path=path)
  resp <- GET(url,
              authenticate(client$api_key, ""),
              add_headers(user_agent=client$ua))

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
    class = "api_get"
  )
}

print.api_get <- function(x, ...){
  cat("<QuantAQ ", x$path, ">\n", sep="")
  str(x$content)
  invisible(x)
}


get_teams <- function(){

}

get_account <- function(){
  api_get("account")
}

get_devices <- function(sn=NULL){
  if (is.null(sn)){
    api_get("devices")
  } else {
    api_get(paste("devices",sn,sep="/"))
  }

}

