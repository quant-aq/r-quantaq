#' Get the user's account information
#'
#' @returns The user's account information
#' @export
whoami <- function(){
  structure(
    request("account"),
    class = "account"
  )
}

#' Convert account class to data.frame
#'
#' @importFrom dplyr mutate
#' @importFrom lubridate parse_date_time
#'
#' @param x The account class returned by \code{\link{whoami()}}
#' @returns A data.frame containing the user's account information
#' @export
as.data.frame.account <- function(x, ...){
  rbind("1" = x) %>% as.data.frame %>%
    dplyr::mutate(across(c(last_seen, member_since), ~lubridate::parse_date_time(.x, "Ymd H:M:S.")))
}

#' Get the user's teams information.
#'
#' @param id The unique numeric identifier for the team
#' @returns The teams information
#' @export
get_teams <- function(id = NULL){
  structure(
    requests(paste("teams", id, sep = "/")),
    class = "teams"
  )
}

#' Converts teams information to data.frame
#'
#' @param x The teams class returned by \link{\code{get_teams()}}
#' @returns A data.frame containing the teams information
#' @export
as.data.frame.teams <- function(x, ...){
  if(!is.null(names(x))){ # if x has names, then there is only one team returned
    x <- list(x)
  }

  return(do.call(rbind, lapply(x, rbind)) %>% as.data.frame)
}

#' Get the user's devices
#'
#' @param sn A device serial number
#' @returns The user's device information
#' @export
get_devices <- function(sn = NULL){
  structure(
    requests(paste("devices", sn, sep = "/")),
    class = "devices"
  )
}

#' Coerce the devices class to a data.frame
#'
#' Converts the class returned by \code{\link{get_devices}()} to a data.frame.
#'
#' @param x The devices class returned by \code{get_devices()}
#' @returns The data from \code{get_devices()} in data.frame format
#' @returns A data.frame containing the devices information
#' @export
as.data.frame.devices <- function(x, ...){
  if(!is.null(names(x))){ # if x has names, then there's only one device returned
    x <- list(x)
  }

  out_df <- do.call(rbind, lapply(x, rbind)) %>%
    as.data.frame %>%
    tidyr::unnest_wider(geo, names_sep = "_") %>%
    tidyr::unnest(everything())

  return(out_df)
}

#' #' Get the device metadata according to specified serial number
#' #'
#' #' @param sn Device serial number
#' #' @returns The device metadata
#' get_device_metadata <- function(sn){
#'   structure(
#'     requests(paste("meta-data", sn, sep="/")),
#'     class = "metadata"
#'   )
#' }

#' Get device data according to provided serial number and other parameters
#'
#' @param sn A device serial number
#' @param limit (Optional) The number of data points to return
#' @param start (Optional) The earliest date to retrieve data from. Should be a timestamp string of the form "YYYY-MM-DD HH:MM:SS"
#' @param stop (Optional) The latest date to retrieve data from. Should be a timestamp string of the form "YYYY-MM-DD HH:MM:SS"
#' @param filter (Optional) A string providing filter parameters, see below examples and \href{https://docs.quant-aq.com/api#8e14edbf9dee4162a04f729ce022cb4b}{API documentation} for more information.
#' @param sort (Optional) A data variable upon which to sort, and the sort method (ascending or descending), formatted as "variable,method", e.g. "timestamp,asc"
#' @param raw (Optional) Returns the raw data. Currently only available to developers and admins.
#'
#' @examples
#' \dontrun{
#' get_data(sn = "MOD-PM-00808", limit = 100)
#' get_data(sn = "MOD-PM-00808", start = "2020-01-01 21:54", stop = "2023-03-03 05:47")
#' get_data(sn = "MOD-PM-00808", filter = "device_state,eq,ACTIVE")
#' get_data(sn = "MOD-PM-00808", sort = "timestamp,asc")
#' }
#'
#' @export
get_data <- function(sn, ...){
  endpoint <- paste("devices", sn, "data", sep = "/")

  if(...length() > 0 && "raw" %in% ...names()){
    endpoint = paste(endpoint, "raw/", sep= "/")
  }

  structure(
    requests(endpoint, httr::GET, ...),
    class = "device_data"
  )
}
#' A convenience function for \code{get_data()} to unnest recursively nested data frames.
#' @importFrom dplyr select where unnest_wider
#'
#' @param df A recursively-nested data frame
#'
#' @returns A flattened data frame, one row per observation
#'
unnest_all <- function(df) {
  list_columns <- df %>% select(where(is.list)) %>% names

  if (length(list_columns) == 0) {
    return(df)
  }

  for (list_column in list_columns) {
    df <- df %>% unnest_wider(list_column, names_sep = "_")
  }
  unnest_all(df)
}


#' Converts the device data class to a data.frame.
#'
#' @import tidyr
#' @importFrom lubridate parse_date_time ymd_hms
#'
#'
#' @param data The data returned from \code{\link{get_data()}}.
#'
#' @returns The data from \code{get_data()} in data.frame format.
#' @export
as.data.frame.device_data <- function(x, ...){
  if(!is.null(names(x))){ # if x has names, then there's only one row of data returned
    x <- list(x)
  }

  do.call(rbind, lapply(x, rbind)) %>%
    as.data.frame() %>%
    unnest_all() %>%
    dplyr::rename_with(~gsub("_1$", "", .x)) %>% # remove "_1" suffix for all columns
    mutate(across(starts_with("timestamp"), ~lubridate::ymd_hms(.x)))
}
