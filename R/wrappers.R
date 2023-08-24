#' A convenience function to unnest recursively nested data frames.
#' @importFrom dplyr select where
#' @importFrom tidyr unnest_wider
#' @importFrom magrittr `%>%`
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

#' Coerce accounts class to data.frame
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate across
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate parse_date_time
#'
#' @param x The account class returned by [whoami()]
#' @param ... Placeholder for passing through to as.data.frame.default
#' @returns A data.frame of the user's account information
#' @export
as.data.frame.account <- function(x, ...){
  rbind("1" = x) %>% as.data.frame %>%
    mutate(across(c("last_seen", "member_since"), ~parse_date_time(.x, "Ymd H:M:S.")))
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
#' @importFrom magrittr `%>%`
#'
#' @param x The teams class returned by [get_teams()]
#' @param ... Placeholder for passing through to as.data.frame.default
#' @returns A data.frame of teams information
#' @export
as.data.frame.teams <- function(x, ...){
  if(!is.null(names(x))){ # if x has names, then there is only one team returned
    x <- list(x)
  }

  return(do.call(rbind, lapply(x, rbind)) %>% as.data.frame)
}

#' Get the user's devices
#'
#' @importFrom httr GET
#' @importFrom wrapr stop_if_dot_args
#'
#' @param sn A device serial number
#' @param ... Named arguments, passed as query parameters (see: limit, sort)
#' @param limit (optional) The number of devices to return
#' @param sort (optional) A parameter upon which to sort, and the sort method (ascending or descending), formatted as "parameter,order", e.g. "id,asc"
#'
#' @returns The user's device information
#' @export
get_devices <- function(sn = NULL, ...){
  structure(
    requests(paste("devices", sn, sep = "/"), verb = httr::GET, ...),
    class = "devices"
  )
}

#' Coerce the devices class to a data.frame
#'
#' Converts the class returned by [get_devices()] to a data.frame.
#'
#' @importFrom magrittr `%>%`
#' @importFrom tidyr unnest unnest_wider
#' @importFrom dplyr rename_with
#' @param x The devices class returned by [get_devices()]
#' @param ... Placeholder for passing through to as.data.frame.default
#' @returns A data.frame of device information
#' @export
as.data.frame.devices <- function(x, ...){
  if(!is.null(names(x))){ # if x has names, then there's only one device returned
    x <- list(x)
  }

  out_df <- do.call(rbind, lapply(x, rbind)) %>%
    as.data.frame %>%
    unnest_all() %>%
    rename_with(~gsub("_1$", "", .x))

  return(out_df)
}

#' Get device metadata
#'
#' @param sn Device serial number
#' @returns The device metadata
get_device_metadata <- function(sn){
  structure(
    requests(paste("meta-data", sn, sep="/")),
    class = "metadata"
  )
}

#' Get device data
#'
#' Get data according to provided serial number and other parameters.
#'
#' @importFrom httr GET
#'
#' @param sn A device serial number
#' @param ... Named arguments passed as query parameters (see limit, start, stop, etc.)
#' @param limit The number of data points to return
#' @param start (optional) The earliest date to retrieve data from. Should be a timestamp string of the form "YYYY-MM-DD HH:MM:SS"
#' @param stop (optional) The latest date to retrieve data from. Should be a timestamp string of the form "YYYY-MM-DD HH:MM:SS"
#' @param filter (optional) A string providing filter parameters, see below examples and \href{https://docs.quant-aq.com/api#8e14edbf9dee4162a04f729ce022cb4b}{API documentation} for more information.
#' @param sort (optional) A data variable upon which to sort, and the sort method (ascending or descending), formatted as "parameter,order", e.g. "timestamp,asc"
#' @param raw (optional) Returns the raw data. Currently only available to developers and admins.
#' @param by_date (optional) A date for which to return data. Must be in format "YYYY-MM-DD".
#'
#' @examples
#' \dontrun{
#'   get_data(sn = "MOD-PM-00808", limit = 100)
#'   get_data(sn = "MOD-PM-00808", start = "2020-01-01 21:54", stop = "2023-03-03 05:47")
#'   get_data(sn = "MOD-PM-00808", filter = "device_state,eq,ACTIVE")
#'   get_data(sn = "MOD-PM-00808", sort = "timestamp,asc")
#' }
#'
#' @returns The specified device data
#' @export
get_data <- function(sn, ...){
  endpoint <- paste("devices", sn, "data", sep = "/")

  if(...length() > 0){
    if("by_date" %in% ...names()){
      endpoint <- paste(endpoint, "data-by-date/", by_date, sep= "/")
    }

    if("raw" %in% ...names()){
      endpoint <- paste(endpoint, "raw/", sep= "/")
    }
  }

  structure(
    requests(endpoint, httr::GET, ...),
    class = "device_data"
  )
}

#' Coerce device data to a data.frame
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr rename_with mutate across starts_with select everything
#' @importFrom lubridate parse_date_time
#'
#' @param x The data returned from [get_data()].
#' @param ... Placeholder for passing through to as.data.frame.default
#'
#' @returns A data.frame of device data
#' @export
as.data.frame.device_data <- function(x, ...){
  if(!is.null(names(x))){ # if x has names, then there's only one row of data returned
    x <- list(x)
  }

  do.call(rbind, lapply(x, rbind)) %>%
    as.data.frame() %>%
    unnest_all() %>%
    rename_with(~gsub("_1$", "", .x)) %>% # remove "_1" suffix for all columns
    mutate(across(starts_with("timestamp"), ~parse_date_time(.x, "Ymd H:M:S."))) %>%
    select("timestamp", everything())
}

#' Get device log data.
#'
#' @param sn Device serial number
#' @param ... Named arguments
#' @returns Data logs
get_logs <- function(sn, ...){
  structure(
    requests(paste("log", sn, sep = "/"), ...),
    class = "logs"
  )
}

#' Get device calibration models
#'
#' @param sn Device serial number
#' @returns Device calibration models
#' @export
get_models <- function(sn){
  structure(
    requests(paste("calibration-models", sn, sep="/")),
    class = 'calibration_models'
  )
}

#' Coerce calibration models to data.frame
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr rename_with rename
#' @importFrom tidyr pivot_longer matches
#'
#' @param x The calibration_models class returned by [get_models()]
#' @param ... Placeholder for passing through to as.data.frame.default
#' @returns A data.frame of calibration_models
#' @export
as.data.frame.calibration_models <- function(x, ...){
  do.call(rbind, lapply(x,rbind)) %>%
    as.data.frame %>%
    unnest_all() %>%
    rename_with(~gsub("_1$", "", .x)) %>%  # remove "_1" suffix for all columns
    rename("model_features_1" = "model_features") %>%
    pivot_longer(
      matches("model_(features|params)"),
      names_pattern = "model_(features|params)_([a-z0-9_]+)",
      names_to = c("element", "element_id"),
      values_transform = as.character
    )
}
