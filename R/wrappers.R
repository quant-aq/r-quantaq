# TODO:
# - data_to_dataframe <- function(){}
#   -make every specific get request return am object, and then provide a "as.data.frame" for each of them?
# - add limit, sort, filter, any other query params

whoami <- function(){
  requests("account")

  # inital pass at to df:
  # whoami() %>% as.data.frame() %>% mutate(across(c(last_seen, member_since), ~lubridate::parse_date_time(.x, "Ymd H:M:S.")))
}

get_teams <- function(id = NULL){
  requests(paste("teams", id, sep = "/"))
}

get_devices <- function(sn = NULL){
  requests(paste("devices", sn, sep = "/"))
}

get_device_metadata <- function(sn = NULL){
  requests(paste("meta-data", sn, sep="/"))
}

get_data <- function(sn, ...){
  endpoint <- paste("devices", sn, "data", sep = "/")

  if("raw" %in% ...names()){
    endpoint = paste(endpoint, "raw/", sep= "/")
  }

  request(endpoint)
}

#' Convert \code{get_data()} list to a dataframe.
#'
#' @importFrom purrr list_flatten
#'
#' @param data The data returned from \code{\link{get_data()}}.
#'
#' @returns The data from \code{get_data()} in data.frame format.
#'
get_data_to_df <- function(data){
  flat_df <- data %>% purrr::list_flatten %>% purrr::list_flatten %>% as.data.frame

  flat_df %>%
    rename_with(~ gsub("\\.([0-9]+$)", "__\\1", .x)) %>%
    rename_with(~gsub("$", "__0", .x), matches("[a-z]([0-9]+)?$")) %>%
    pivot_longer(
      everything(),
      names_to =c(".value", "ind"),
      names_pattern = "(^.*)(__[0-9]+$)"
    ) %>%
    mutate(across(ind, ~ gsub("__", "", .x) %>% as.numeric)) %>%
    select(timestamp, everything()) %>%
    mutate(across(c(timestamp, timestamp_local), ~lubridate::parse_date_time(.x, "Y-mdH:M:S"))) %>%
    arrange(across(timestamp)) %>%
    select(-ind)
}
