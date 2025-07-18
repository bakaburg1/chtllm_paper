% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate.R
\name{report_status_distribution}
\alias{report_status_distribution}
\title{Report status distribution of temporary results}
\usage{
report_status_distribution(
  type = c("processed", "processing"),
  results_dir = here::here("results")
)
}
\arguments{
\item{type}{The type of results to analyze, either "processed" or
"processing".}

\item{results_dir}{The base directory containing results subdirectories.}
}
\value{
A named integer vector with counts for each status.
}
\description{
Analyzes the distribution of result statuses from temporary result files.
Status codes are:
\itemize{
\item C: Correct answer
\item F: Failed/incorrect answer
\item E: Error occurred
\item N: No valid answer extracted
}
}
