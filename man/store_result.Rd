% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{store_result}
\alias{store_result}
\title{Store a single result row in the results file}
\usage{
store_result(result_combination, path = here::here("results", "results.csv"))
}
\arguments{
\item{result_combination}{A data.frame containing a single result row.}

\item{path}{Path to the CSV file to store results.}
}
\value{
The result row as a tibble, invisibly.
}
\description{
This function is designed for concurrent access from multiple processes. It
uses file locking to ensure atomic writes when multiple agents try to append
results simultaneously.
}
