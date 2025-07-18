% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{load_results}
\alias{load_results}
\title{Load results from CSV file}
\usage{
load_results(path = here::here("results", "all_results.csv"))
}
\arguments{
\item{path}{Path to the CSV file containing results.}
}
\value{
A tibble containing the results data with columns for item ID, model
ID, modality, question text, correct option, raw LLM response, processed
answer, timestamp, generation time, and token counts.
}
\description{
Load results from CSV file
}
