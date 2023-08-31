# r-quantaq

An R wrapper for the QuantAQ RESTful API.

**NOTE**:
As of Aug 30th, 2023, this package is in **active development** and will be updated in potentially breaking ways until the first release.

# Installation

Currently, you must install directly from GitHub:

```R
# If you don't have devtools installed:
install.packages("devtools")

# Install the library from GitHub
devtools::install_github(repo="quant-aq/r-quantaq")
```


# Docs

Complete documentation is available [here](). A quick-start guide follows:

# Getting Started

To use `r-quantaq`, import it:

```R
library("QuantAQAPIClient")
```

## Authentication

To use the QuantAQ API, you must have an API key. To obtain an API key, please follow the instructions [here](https://docs.quant-aq.com/api#541cc94f0c3a41abbb0200b69bb8d9e2).

To begin, call `setup_client()` to set your API Key. By passing `verbose = TRUE`, you will receive a message on successful connection to the API. It's possible, but not recommended, to include your API Key in the function as an argument, (i.e., `setup_client(api_key = <your api key here>)`). We recommend that RStudio users call `setup_client()` without arguments, and RStudio will use its automatic secret key manager. You can then follow the prompt to install the keyring package, which saves the key so you don't have to enter it every time.

## Utility Functions

To receive data as a dataframe (rather than json), you can use the `as.data.frame` function:

```R
# Return data as a dataframe
as.data.frame(get_data("<serial-number>"))

# Pipe it through
get_data("<serial-number>") %>% as.data.frame()
```


## Basic Functions

### `whoami()`

`whoami()` returns your basic account information.

```R
# Get your account information
account <- whoami()
```

### `get_data(sn, limit=100, start=NULL, stop=NULL, filter=NULL, sort=NULL, raw=FALSE)`

Return all data for a device by its serial number. 

```R
# Get the data for a device with all defaults
x <- as.data.frame(get_data("<serial-number>"))

# Get the first 10 records for a device 
x <- as.data.frame(get_data("<serial-number>", limit=10))

# Get the raw data
x <- as.data.frame(get_data("<serial-number>", raw=TRUE))
```

### `get_teams()`

Return a list of all teams that you belong to.

```R
# Get the list of teams
x <- get_teams()
```

### `get_devices(sn=NULL, limit=NULL, sort=NULL)`

Get a list of all devices you have access to.

```R
# Get a list of the first 25 devices as a dataframe
x <- as.data.frame(get_devices(limit=25))
```

### `get_data_by_date(sn, date, raw=FALSE)`

Return the data for a device by the date. This is the most efficient way to grab a lot of data as it is cached and ready to download.

```R
# Get the data for date as a dataframe
x <- as.data.frame(get_data_by_date("<serial-number>", "2023-08-30"))

# Get the raw data for date as a dataframe
x <- as.data.frame(get_data_by_date("<serial-number>", "2023-08-30", raw=TRUE))
```

## Advanced Queries

Advanced requests are available by named keywords, examples below. See the [API docs](https://docs.quant-aq.com/api#1bcd5e949cb74e63ab25d214d600e1af) for more information.

## Filter
Format as `filter = '<parameter>,<filter spec>,<value>;<parameter>,<filter spec>,<value>;'`. Append as many filters as desired.

```R
# returns data where pm25 >= 25 and <= 50
get_data("<serial-number>", filter = 'pm25,ge,25;pm25,le,50') 

# returns only data where device is on, i.e. device_state == ACTIVE
get_data("<serial-number>", filter = "device_state,eq,ACTIVE") 
```

## Limit 
Format as `limit = <value>`.

```R
# returns 25 devices
get_devices(limit = 25) 

# returns first 10 timestamps of data
get_data("<serial-number>"), limit = 10) 
```

## Sort

Include `sort = '<parameter><order>'`, where 'order' is one of "asc" or "desc".

```R
# returns data in descending order by timestamp
get_data("<serial-number>"), sort = 'timestamp,desc') 
```




, and each `get_` function GETs the data from its similarly-named API endpoint. Each of these can be piped or passed to `as.data.frame` for nicer formatting, e.g. to get data from device serial number `MOD-PM-00233`:



# Development

Tests are run automatically when pull requests are made against the `main` branch via a github action. To run locally:

```R
devtools::test()
```

To build a new version of the docs pdf:

```R
devtools::document() # in case any changes were made to the documentation in the code
devtools::build_manual() # to build the pdf
```

To contribute to this library, please submit a pull request with documentation and notes about what changed and why. Please be as descriptive as possible, add tests where necessary, and add an appropriate label. A QuantAQ maintainer will review and merge PRs as they are received.

If you notice any bugs or have questions about this library, please create an Issue using the GitHub Issues feature.
