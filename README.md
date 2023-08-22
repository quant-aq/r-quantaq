# r-quantaq
The official R wrapper for the QuantAQ API

**In development**

# Dev Version Install

```R
install.packages("devtools")
devtools::install_github(repo=quantaq/r-quantaq')
```

# General Setup
To connect to the API, first run `setup_client()`. By passing `verbose = TRUE`, you will receive a message on successful connection to the API.

| :memo:        |  Though you can include the api key in the function signature, (i.e. `setup_client(api_key = <your api key here>)`), we recommend RStudio users to call `setup_client()` without arguments, and RStudio will use its automatic secret key manager. You can use this to save the key so you don't have to enter it next time. |
|---------------|:------------------------|
