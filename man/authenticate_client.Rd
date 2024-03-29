% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{authenticate_client}
\alias{authenticate_client}
\title{Authenticate API connection}
\usage{
authenticate_client(verbose = FALSE)
}
\arguments{
\item{verbose}{(Optional) Default FALSE. Whether you want a message printed upon API connection.}
}
\value{
No return value. \code{verbose = TRUE} causes a message to print to console upon successful API connection.
}
\description{
A helper function for \code{\link[=setup_client]{setup_client()}} to determine that the user-provided
API details yields a proper connection with the API.
}
