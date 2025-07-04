% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{generate_temp_filename}
\alias{generate_temp_filename}
\title{Generate temp file name for a result}
\usage{
generate_temp_filename(
  data = NULL,
  item = NULL,
  model_id = NULL,
  modality = NULL,
  status = NULL,
  replication = 1L,
  dir = NULL
)
}
\arguments{
\item{data}{A data frame containing item, model_id, modality, status, and
replication columns, or NULL if providing individual vectors.}

\item{item}{Vector of question item IDs. Ignored if data is provided.}

\item{model_id}{Vector of model IDs. Ignored if data is provided.}

\item{modality}{Vector of query modalities. Ignored if data is provided.}

\item{status}{Vector of result statuses (C = correct, F = fail, N = not
parsable, E = error). Ignored if data is provided.}

\item{replication}{Vector of replication numbers. Ignored if data is
provided.}

\item{dir}{Directory to store temp files.}
}
\value{
Vector of full paths to the temp files.
}
\description{
Generate temp file name for a result
}
