#' Report status distribution of temporary results
#'
#' Analyzes the distribution of result statuses in the temporary results directory.
#' Status codes are:
#' - C: Correct answer
#' - F: Failed/incorrect answer
#' - E: Error occurred
#' - N: No valid answer extracted
#'
#' @param temp_results_dir The directory containing temporary results.
#'
#' @return A named integer vector with counts for each status.
#'
#' @export
report_status_distribution <- function(
  temp_results_dir = here::here("results", "temp")
) {
  # Get all temp result files
  files <- list.files(
    temp_results_dir,
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

