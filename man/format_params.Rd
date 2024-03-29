% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_calls.R
\name{format_params}
\alias{format_params}
\title{Handle requests params}
\usage{
format_params(...)
}
\arguments{
\item{...}{Parameters to be translated to query string and passed to the request.}
}
\value{
A named list of query params.
}
\description{
A helper function to format the parameters that can be passed to \code{\link[=requests]{requests()}}.
}
