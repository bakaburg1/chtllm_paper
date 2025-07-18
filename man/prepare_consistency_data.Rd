% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze.R
\name{prepare_consistency_data}
\alias{prepare_consistency_data}
\title{Prepare data for consistency model (binomial)}
\usage{
prepare_consistency_data(results)
}
\arguments{
\item{results}{Raw results data from targets pipeline.}
}
\value{
A tibble with columns \code{item}, \code{model_id}, \code{modality}, \code{y}, and
\code{total}, suitable for fitting a binomial model via \code{y | trials(total)}.
}
\description{
Aggregates results to compute, for each (item, model, modality) cell, the
number of \emph{distinct} answers produced (\code{y}) out of the total number of
responses (\code{total}). A value of \code{y = 1} indicates perfect consistency (the
model always produced the same answer), while larger values indicate lower
consistency. Error (\code{"E"}) and not-found (\code{"N"}) records are excluded.
}
