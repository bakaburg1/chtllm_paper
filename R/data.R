#' Load questions from CSV file
#'
#' @param path Path to the CSV file.
#'
#' @return A tibble containing the questions data.
#'
#' @export
load_questions <- function(
  path = here::here("inst", "benchmark_travel_medicine.csv")
) {
  readr::read_csv(path, show_col_types = FALSE) |>
    mutate_all(as.character) |>
    # Validate data
    validate_questions()
}

#' Load model configurations
#'
#' @param path Path to the CSV file containing model configurations.
#'
#' @return A tibble containing the model configurations.
#'
#' @export
load_models <- function(
  path = here::here("inst", "models.csv")
) {
  readr::read_csv(path, show_col_types = FALSE) |>
    validate_models()
}

#' Format a question for LLM consumption
#'
#' @param item_text The question text.
#' @param options Named list of options (A, B, C, D).
#'
#' @return A character string containing the formatted question.
#'
#' @keywords internal
format_question <- function(item_text, options) {
  if (is.null(options)) return(item_text)

  sprintf(
    "Question: %s\n\nOptions:\nA: %s\nB: %s\nC: %s\nD: %s",
    item_text,
    options$A,
    options$B,
    options$C,
    options$D
  )
}

#' Extract and format a question from the benchmark data
#'
#' @param questions A tibble containing questions data, as returned by
#'   `load_questions()`.
#' @param item The item ID or row number of the question to extract.
#'
#' @return A character string containing the formatted question ready for LLM
#'   consumption.
#'
#' @export
get_formatted_question <- function(
  item,
  questions = load_questions()
) {
  # Extract the question row
  question <- if (is.numeric(item)) {
    # If item is numeric, treat as row number
    if (!item %in% seq_len(nrow(questions))) {
      cli::cli_abort(
        paste(
          "Row number {item} exceeds number of questions",
          "({nrow(questions)})"
        )
      )
    }
    questions[item, ]
  } else {
    # Otherwise treat as item ID
    questions |>
      dplyr::filter(.data$item == !!item)
  }

  # Check if we found a question
  if (nrow(question) == 0) {
    cli::cli_abort("Question with item ID {item} not found")
  }

  # Warn if multiple questions found
  if (nrow(question) > 1) {
    cli::cli_warn(
      "Multiple questions found with item ID {item}. Using first one."
    )
  }

  # Extract options into a list
  options <- list(
    A = question$option_A[1],
    B = question$option_B[1],
    C = question$option_C[1],
    D = question$option_D[1]
  )

  # Format the question
  format_question(question$item_text[[1]], options)
}

#' Validate questions data
#'
#' @param questions Tibble containing questions data.
#'
#' @return The validated questions tibble.
#'
#' @keywords internal
validate_questions <- function(questions) {
  # Check required columns
  required_cols <- c(
    "item",
    "source",
    "source_link",
    "item_text",
    "option_A",
    "option_B",
    "option_C",
    "option_D",
    "option_correct"
  )

  missing_cols <- setdiff(required_cols, names(questions))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in questions data:",
      missing_cols
    ))
  }

  # Validate correct answers
  invalid_answers <- questions |>
    filter(!.data$option_correct %in% LETTERS[1:4])

  if (nrow(invalid_answers) > 0) {
    cli::cli_abort(c(
      "Invalid correct answers found:",
      invalid_answers$item
    ))
  }

  questions
}

#' Validate model configurations
#'
#' @param models Tibble containing model configurations.
#'
#' @return The validated model configurations tibble.
#'
#' @keywords internal
validate_models <- function(models) {
  # Check required columns
  required_cols <- c(
    "model_id",
    "provider",
    "model",
    "model_type",
    "active"
  )

  if (any(duplicated(models$model_id))) {
    model_table <- table(models$model_id)
    cli::cli_abort(c(
      "Duplicated model ids:",
      paste(names(model_table)[model_table > 1], collapse = ", ")
    ))
  }

  missing_cols <- setdiff(required_cols, names(models))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in model configurations:",
      missing_cols
    ))
  }

  # Validate providers
  # Get list of available providers from ellmer package
  valid_providers <- ls(asNamespace("ellmer"), pattern = "^chat_") |>
    stringr::str_remove("^chat_") |>
    c("openrouter") # OpenRouter is not in ellmer, so we add it manually

  invalid_providers <- models |>
    filter(!.data$provider %in% valid_providers)

  if (nrow(invalid_providers) > 0) {
    cli::cli_abort(c(
      "Invalid providers found:",
      invalid_providers$provider
    ))
  }

  # Validate model types
  valid_types <- c("reasoning", "classic")
  invalid_types <- models |>
    filter(!.data$model_type %in% valid_types)

  if (nrow(invalid_types) > 0) {
    cli::cli_abort(c(
      "Invalid model types found:",
      invalid_types$model_id
    ))
  }

  # Validate active field
  invalid_active <- models |>
    filter(!.data$active %in% c(0, 1))

  if (nrow(invalid_active) > 0) {
    cli::cli_abort(c(
      "Invalid active values found (must be 0 or 1):",
      invalid_active$model_id
    ))
  }

  models
}

#' Load results from CSV file
#'
#' @param path Path to the CSV file containing results.
#'
#' @return A tibble containing the results data with columns for item ID, model
#'   ID, modality, question text, correct option, raw LLM response, processed
#'   answer, timestamp, generation time, and token counts.
#'
#' @export
load_results <- function(
  path = here::here("results", "all_results.csv")
) {
  # Check if file exists
  if (!file.exists(path)) {
    # Return empty tibble with correct structure
    return(tibble::tibble(
      item = character(),
      model_id = character(),
      modality = character(),
      item_text = character(),
      option_correct = character(),
      raw_response = character(),
      answer = character(),
      timestamp = as.POSIXct(character()),
      generation_time = numeric(),
      input_tokens = integer(),
      output_tokens = integer(),
      total_tokens = integer()
    ))
  }

  # Read and validate results
  results <- readr::read_csv(path, show_col_types = FALSE) |>
    # Ensure all required columns are present
    validate_results()

  results
}

#' Store a single result row in the results file
#'
#' This function is designed for concurrent access from multiple processes. It
#' uses file locking to ensure atomic writes when multiple agents try to append
#' results simultaneously.
#'
#' @param result_combination A data.frame containing a single result row.
#' @param path Path to the CSV file to store results.
#'
#' @return The result row as a tibble, invisibly.
#'
#' @export
store_result <- function(
  result_combination,
  path = here::here("results", "results.csv")
) {
  # Convert input to tibble if it isn't already
  result_row <- tibble::as_tibble(result_combination)

  # Verify we have exactly one row
  if (nrow(result_row) != 1) {
    cli::cli_abort("result_combination must contain exactly one row")
  }

  # Validate the result row
  validate_results(result_row)

  # Create directory if it doesn't exist
  dir.path <- dirname(path)
  if (!dir.exists(dir.path)) {
    dir.create(dir.path, recursive = TRUE)
  }

  # Create empty file with headers if it doesn't exist
  if (!file.exists(path)) {
    readr::write_csv(
      tibble::tibble(
        item = character(),
        model_id = character(),
        modality = character(),
        item_text = character(),
        option_correct = character(),
        raw_response = character(),
        answer = character(),
        timestamp = as.POSIXct(character()),
        generation_time = numeric(),
        input_tokens = integer(),
        output_tokens = integer(),
        total_tokens = integer()
      ),
      path
    )
  }

  # Use filelock to ensure atomic append
  lock <- filelock::lock(paste0(path, ".lock"))
  on.exit(filelock::unlock(lock))

  # Append the result
  readr::write_csv(
    result_row,
    path,
    append = TRUE
  )

  invisible(result_row)
}

#' Validate results data
#'
#' @param results Tibble containing results data.
#'
#' @return The validated results tibble.
#'
#' @keywords internal
validate_results <- function(results) {
  # Check required columns
  required_cols <- c(
    "item",
    "model_id",
    "modality",
    "item_text",
    "option_correct",
    "raw_response",
    "answer",
    "timestamp",
    "generation_time",
    "input_tokens",
    "output_tokens",
    "total_tokens"
  )

  missing_cols <- setdiff(required_cols, names(results))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in results data:",
      missing_cols
    ))
  }

  # Validate modalities
  invalid_modalities <- results |>
    filter(!.data$modality %in% c("cold", "free", "reasoning"))

  if (nrow(invalid_modalities) > 0) {
    cli::cli_abort(c(
      "Invalid modalities found:",
      invalid_modalities$modality |> unique()
    ))
  }

  values <- c(LETTERS[1:4], "NONE", "ERROR")

  # Validate answers (both processed and expected)
  invalid_answers <- results |>
    filter(!.data$answer %in% values)

  if (nrow(invalid_answers) > 0) {
    cli::cli_abort(c(
      "Invalid answers found (must be A/B/C/D or NONE/ERROR):",
      "Rows:",
      invalid_answers$item
    ))
  }

  results
}

#' Load results data from temp files
#'
#' @param dir Directory containing temp files.
#' @param enrich Whether to enrich the results with model configuration
#' @param models_path Path to the models CSV file used for enrichment.
#'
#' @return If as = "data.frame", returns a tibble containing all results
#'   combined. If as = "list", returns a list of tibbles, one per file.
#'
#' @export
compile_results_data <- function(
  dir = here::here("results", "processed"),
  enrich = TRUE,
  models_path = here::here("inst", "models.csv")
) {
  # If directory doesn't exist, return empty result
  if (!dir.exists(dir)) {
    return(tibble::tibble(
      item = character(),
      model_id = character(),
      modality = character(),
      replication = integer(),
      item_text = character(),
      option_correct = character(),
      raw_response = character(),
      answer = character(),
      status = character(),
      timestamp = as.POSIXct(character()),
      generation_time = numeric(),
      input_tokens = integer(),
      output_tokens = integer(),
      total_tokens = integer()
    ))
  }

  # List all CSV files
  files <- list.files(
    dir,
    pattern = "^[CFNE]\\..+\\.csv$",
    full.names = TRUE
  )

  # Read all files
  #mirai::daemons(parallel::detectCores() - 1)
  #on.exit(mirai::daemons(0))
  #processor <- mirai::mirai_map(
  results <- purrr::map(
    files,
    \(f) {
      data <- readr::read_csv(f, show_col_types = FALSE, progress = FALSE)

      if (nrow(data) == 0) {
        file.remove(f)
        f_data <- basename(f) |>
          stringr::str_remove("^\\w\\.") |> # Remove status
          stringr::str_remove("\\.csv$") |> # Remove extension
          stringr::str_split_1("_")

        result <- data.frame(
          item = f_data[1],
          model_id = f_data[2],
          modality = f_data[3],
          replication = f_data[4] |> stringr::str_remove("rep"),
          item_text = NA,
          option_correct = NA,
          raw_response = "File is empty",
          answer = NA,
          status = "E",
          timestamp = NA,
          generation_time = NA,
          input_tokens = NA,
          output_tokens = NA,
          total_tokens = NA
        )
        return(result)
      }

      data |>
        dplyr::mutate_all(as.character) |>
        # Fix the status, since it's missing in older files
        dplyr::mutate(
          status = determine_status(.data$answer, .data$option_correct),
          filename = basename(f)
        )
    },
    #determine_status = determine_status
    .progress = TRUE
  ) |>
    dplyr::bind_rows()

  # results <- processor[mirai::.progress] |>
  #   dplyr::bind_rows()

  #mirai::daemons(0)

  # Enrich results with model configuration if requested
  if (enrich) {
    models <- load_models(models_path)
    results <- results |>
      dplyr::left_join(models, by = "model_id")
  }
}

#' Generate temp file name for a result
#'
#' @param data A data frame containing item, model_id, modality, status, and
#'   replication columns, or NULL if providing individual vectors.
#' @param item Vector of question item IDs. Ignored if data is provided.
#' @param model_id Vector of model IDs. Ignored if data is provided.
#' @param modality Vector of query modalities. Ignored if data is provided.
#' @param status Vector of result statuses (C = correct, F = fail, N = not
#'   parsable, E = error). Ignored if data is provided.
#' @param replication Vector of replication numbers. Ignored if data is
#'   provided.
#' @param dir Directory to store temp files.
#'
#' @return Vector of full paths to the temp files.
#'
#' @export
generate_temp_filename <- function(
  data = NULL,
  item = NULL,
  model_id = NULL,
  modality = NULL,
  status = NULL,
  replication = 1L,
  dir = NULL
) {
  # If data is provided, check required columns and extract vectors
  if (!is.null(data)) {
    required_cols <- c(
      "item",
      "model_id",
      "modality",
      "status",
      "replication"
    )
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
      cli::cli_abort(c(
        "Missing required columns in data:",
        missing_cols
      ))
    }

    item <- data$item
    model_id <- data$model_id
    modality <- data$modality
    status <- data$status
    replication <- data$replication %||% 1L
  }

  # Validate status (T is for temporary files)
  if (!all(status %in% c("C", "F", "N", "E", "T"))) {
    cli::cli_abort("Invalid status found")
  }

  # Create directory if it doesn't exist
  if (!is.null(dir) && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  # Generate file names
  list(
    dir,
    sprintf(
      "%s.%s_%s_%s_rep%d.csv",
      status,
      item,
      model_id,
      modality,
      replication
    )
  ) |>
    purrr::compact() |>
    do.call(what = "file.path")
}

#' Store a single result in a temp file
#'
#' @param result_combination A data.frame containing a single result row.
#' @param status Result status (C = correct, F = fail, N = not parsable, E =
#'   error).
#' @param dir Directory to store temp files.
#'
#' @return The temp file path, invisibly.
#'
#' @export
store_temp_result <- function(
  result_combination,
  status = NULL,
  dir = here::here("results", "temp")
) {
  # Convert input to tibble if it isn't already
  result_row <- tibble::as_tibble(result_combination)

  # Verify we have exactly one row
  if (nrow(result_row) != 1) {
    cli::cli_abort("result_combination must contain exactly one row")
  }

  # Override status if provided
  result_row <- result_row |>
    mutate(status = coalesce(status, .env$status))

  # Generate temp file name
  temp_file <- generate_temp_filename(
    data = result_row,
    dir = dir
  )

  # Write result to temp file
  readr::write_csv(result_row, temp_file)

  invisible(temp_file)
}
