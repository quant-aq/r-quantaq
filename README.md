# r-quantaq
The official R wrapper for the QuantAQ API

**In development**

# Dev Version Install

```R
install.packages("devtools")
devtools::install_github(repo="quantaq/r-quantaq"")
```

# General Setup
To connect to the API, first run `setup_client()`. By passing `verbose = TRUE`, you will receive a message on successful connection to the API.

Though you can include the api key in the function signature, (i.e. `setup_client(api_key = <your api key here>)`), we recommend RStudio users to call `setup_client()` without arguments, and RStudio will use its automatic secret key manager. You can then follow the prompt to install the keyring package, which saves the key so you don't have to enter it every time.

# Basic Functioning
`whoami()` returns your account information, and each `get_` function GETs the data from its similarly-named API endpoint. Each of these can be piped or passed to `as.data.frame` for nicer formatting, e.g.

```
as.data.frame(get_data("MOD-PM-00233"))
get_data("MOD-PM-00233") %>% as.data.frame()
```
both return the data from device serial number MOD-PM-00233.

# Advanced Queries

Advanced requests are available by named keywords, examples below. See [API man web page](https://docs.quant-aq.com/api#1bcd5e949cb74e63ab25d214d600e1af) for more information.

## Filter

```
# filter = '<parameter>,<filter spec>,<value>;<parameter>,<filter spec>,<value>;'
get_data("MOD-PM-002233", filter = 'pm25,ge,25;pm25,le,50') # returns data where pm25 is greater than or equal to 25 and less than or equal to 50
```

## Limit 

```
# limit = <value>
get_devices(limit = 25) # returns 25 devices
get_data("MOD-PM-00233"), limit = 10) # returns first 10 timestamps of data
```

## Sort
```
# sort = '<parameter><order>', where 'order' is one of "asc" or "desc" 
get_data("MOD-PM-00233"), sort = 'timestamp,desc') # returns data in descending order by timestamp
```

