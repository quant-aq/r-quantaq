# TODO:
# - data_to_dataframe <- function(){}
#   -make every specific get request return am object, and then provide a "as.data.frame" for each of them?
# - add limit, sort, filter, any other query params

whoami <- function(){
  requests("account")
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

# TODO: include "raw"
get_data <- function(sn, query_params = NULL, ...){
  # if(args())

  # endpoint <- paste("devices", sn, "data", sep = "/")
  # request(endpoint, query_params = query_params)
}

