#' A convenience function to unnest recursively nested data frames.
#' @importFrom dplyr select where any_of
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
    df <- df %>% unnest_wider(any_of(list_column), names_sep = "_")
  }
  unnest_all(df)
}

#' Get the user's account information
#'
#' @returns The user's account information
#' @export
whoami <- function(){
  structure(
    quantaq_request("account"),
    class = "account"
  )
}

#' Coerce accounts information to data.frame
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate across
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

#' Get the user's organizations information.
#'
#' @param organization_id The unique numeric identifier for the org
#' @return The organizations information
#' @export
get_organizations <- function(organization_id = NULL){
  structure(
    requests(paste("orgs", organization_id, sep = "/")),
    class = "organizations"
  )
}

#' Coerce organizations information to data.frame
#'
#' @importFrom magrittr `%>%`
#'
#' @param x The organizations class returned by [get_organizations()]
#' @param ... Placeholder for passing through to as.data.frame.default
#' @returns A data.frame of organizations information
#' @export
as.data.frame.organizations <- function(x, ...){
  if (!is.null(names(x))) {  # if x has names, then there is only one org
    x <- list(x)
  }

  # we've got a complicated structure, so it's easiest to decide what to do with
  # the nested stuff first (by converting to strings)
  stringified <- lapply(x, function(row) {
    row$devices <- paste(row$devices, collapse=";")
    row$networks <- paste(row$networks, collapse=";")
    members <- lapply(row$members, function(member) {
      return(paste(member$user, "-", member$role))
    })
    row$members <- paste(members, collapse = ";")
    return(row)
  })

  # see: https://stackoverflow.com/a/68162050
  df <- do.call(rbind, lapply(stringified, function(row) {
    # need to unclass the row or when we run data.frame
    # we'll do the whole thing again!
    row %>% unclass %>% data.frame
  }))
  df %>% mutate(created_on = parse_date_time("created_on", "Ymd H:M:S.z"))
}

#' Get the user's networks information, within the context of an organization
#'
#' @param organization_id The unique numeric identifier for the parent org
#' @param network_id The unique numeric identifier for the network
#' @return The networks information
#' @export
get_networks <- function(organization_id, network_id = NULL){
  structure(
    requests(paste("orgs", organization_id, "networks", network_id, sep = "/")),
    class = "networks"
  )
}

#' Coerce networks information to data.frame
#'
#' @importFrom magrittr `%>%`
#'
#' @param x The networks class returned by [get_networks()]
#' @param ... Placeholder for passing through to as.data.frame.default
#' @returns A data.frame of networks information
#' @export
as.data.frame.networks <- function(x, ...){
  if (!is.null(names(x))) {  # if x has names, then there is only one network
    x <- list(x)
  }

  # we've got a complicated structure, so it's easiest to decide what to do with
  # the nested stuff first (by converting to strings)
  stringified <- lapply(x, function(row) {
    row$devices <- paste(row$devices, collapse=";")
    members <- lapply(row$members, function(member) {
      return(paste(member$user, "-", member$role))
    })
    row$members <- paste(members, collapse = ";")
    return(row)
  })

  # see: https://stackoverflow.com/a/68162050
  df <- do.call(rbind, lapply(stringified, function(row) {
    # need to unclass the row or when we run data.frame
    # we'll do the whole thing again!
    row %>% unclass %>% data.frame
  }))
  df %>% mutate(created_on = parse_date_time("created_on", "Ymd H:M:S.z"))
}

#' Get the user's devices
#'
#' @importFrom wrapr stop_if_dot_args
#'
#' @param sn A device serial number
#' @param limit (optional) The number of devices to return
#' @param sort (optional) A parameter upon which to sort, and the sort method (ascending or descending), formatted as `"<parameter>,<order>"`, e.g. `"id,asc"`
#'
#' @returns The user's device information
#' @export
get_devices <- function(sn = NULL, limit = NULL, sort = NULL){
  structure(
    requests(paste("devices", sn, sep = "/"), limit = limit, sort = sort),
    class = "devices"
  )
}

#' Coerce devices information to a data.frame
#'
#' @importFrom magrittr `%>%`
#' @importFrom tidyr unnest unnest_wider
#' @importFrom dplyr rename_with
#'
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
#' @param sn A device serial number
#' @param limit (optional) Default = 1000. The number of data points to return.
#' @param start (optional) The earliest date to retrieve data from. Should be a timestamp string of the form "YYYY-MM-DD HH:MM:SS"
#' @param stop (optional) The latest date to retrieve data from. Should be a timestamp string of the form "YYYY-MM-DD HH:MM:SS"
#' @param filter (optional) A string providing filter details of the form `"<parameter>,<filter spec>,<value>;"`. See below examples and \href{https://docs.quant-aq.com/api#8e14edbf9dee4162a04f729ce022cb4b}{API documentation} for more information.
#' @param sort (optional) A data parameter upon which to sort, and the sort method (ascending or descending), formatted as `"<parameter>,<order>"`, e.g. `"timestamp,asc"`
#' @param raw (optional) Default FALSE. Returns the raw data. Currently only available to developers and admins.
#'
#' @examples
#' \dontrun{
#'   # to change the limit on the amount of data returned
#'   get_data(sn = <device sn>, limit = 100)
#'
#'   # to return data only within specific timestamps
#'   get_data(sn = <device sn>, start = "20203-03-02 21:54", stop = "2023-03-03 05:47")
#'
#'   # to return data only where the device state is ACTIVE
#'   get_data(sn = <device sn>, filter = "device_state,eq,ACTIVE;")
#'
#'   # to return data where PM2.5 is between 25 and 50 µg/m3
#'   get_data(sn = <device sn>, filter = "filter=pm25,ge,25;pm25,le,50;")
#'
#'   # to sort ascending by timestamp
#'   get_data(sn = <device sn>, sort = "timestamp,asc")
#' }
#'
#' @returns The specified device data
#' @export
get_data <- function(sn, limit = 1000, start = NULL, stop = NULL, filter = NULL, sort = NULL, raw = FALSE){
  endpoint <- paste("devices", sn, "data", sep="/")

  if(raw){
    endpoint <- paste(endpoint, "raw/", sep= "/")
  }

  structure(
    requests(endpoint,
             limit = limit,
             start = start,
             stop = stop,
             filter = filter,
             sort = sort
             ),
    class = "device_data"
  )
}

#' Get device data by date
#'
#' Get data according to the provided serial number and date.
#'
#' @param sn A device serial number
#' @param date A date for which to return data. Must be in format "YYYY-MM-DD".
#' @param raw (optional) Default FALSE. Returns the raw data. Currently only available to developers and admins.
#'
#' @returns The specified device data for the specified date
#'
#' @export
get_data_by_date <- function(sn, date, raw = FALSE){
  endpoint <- paste("devices", sn, "data-by-date", sep = "/")

  if(raw){
    endpoint <- paste(endpoint, "raw", sep="/")
  }

  endpoint <- paste(endpoint, date, sep = "/")

  structure(
    requests(endpoint),
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
#' @param limit (optional) The number of logs to return
#' @returns Data logs
get_logs <- function(sn, limit = NULL){
  structure(
    requests(paste("log", sn, sep = "/"), limit = limit),
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
