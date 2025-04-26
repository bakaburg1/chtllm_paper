#' Report status distribution of temporary results
#'
#' Analyzes the distribution of result statuses from temporary result files.
#' Status codes are:
#' - C: Correct answer
#' - F: Failed/incorrect answer
#' - E: Error occurred
#' - N: No valid answer extracted
#'
#' @param type The type of results to analyze, either "processed" or
#'   "processing".
#' @param results_dir The base directory containing results subdirectories.
#'
#' @return A named integer vector with counts for each status.
#'
#' @export
report_status_distribution <- function(
  type = c("processed", "processing"),
  results_dir = here::here("results")
) {
  type <- match.arg(type)

  # Get the directory of the files to process
  files_dir <- file.path(results_dir, type)

  # Get all temp result files
  files <- list.files(
    files_dir,
    pattern = "^[A-Z]\\..+\\.csv$"
  )

  # Extract status codes (first character)
  statuses <- stringr::str_extract(files, "^[A-Z]")

  # Count occurrences of each status
  status_counts <- table(statuses) |>
    as.vector()

  # Name the counts vector
  names(status_counts) <- names(table(statuses))

  status_counts
}

#' Determine result status based on extracted answer and correct answer
#'
#' Status codes are:
#' - C: Correct answer
#' - F: Failed/incorrect answer
#' - E: Error occurred
#' - N: No valid answer extracted
#'
#' @param extracted_answer Vector of answers extracted from LLM responses
#' @param correct_answer Vector of known correct answers
#'
#' @return A character vector indicating the status (C, F, E, or N) for each
#'   answer
#'
#' @export
determine_status <- function(extracted_answer, correct_answer) {
  dplyr::case_when(
    grepl("ERROR", extracted_answer) ~ "E",
    is.na(extracted_answer) | extracted_answer == "" ~ "N",
    grepl("NONE", extracted_answer) ~ "N",
    grepl(correct_answer, extracted_answer) ~ "C",
    .default = "F"
  )
}
