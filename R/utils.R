#' Format a difftime object as a string
#'
#' @param td A difftime object to format.
#' @param precision Number of significant digits to include in the output.
#'
#' @return A character string containing the formatted time difference with
#'   units.
#'
#' @keywords internal
format_timediff <- function(td, precision = 3) {
  # Validate input is a difftime object
  if (!inherits(td, "difftime")) {
    stop("Input must be a difftime object.")
  }

  # Format the time difference with specified precision and units
  paste(signif(unclass(td), precision), units(td))
}

#' Get model configuration from models.csv
#'
#' @param model_id The model ID to look up.
#'
#' @return A list containing the model configuration.
#'
#' @export
get_model_config <- function(model_id) {
  # Read models.csv from inst
  models_path <- here::here("inst", "models.csv")
  if (!file.exists(models_path)) {
    cli::cli_abort("Cannot find models.csv in package data")
  }

  # Read the CSV
  models <- utils::read.csv(models_path, stringsAsFactors = FALSE)

  # Find the model
  model_row <- models[models$model_id == model_id, ]
  if (nrow(model_row) == 0) {
    cli::cli_abort("Model ID {.code {model_id}} not found in models.csv")
  }

  # Convert row to list
  config <- as.list(model_row)

  # Remove row names and model_id (since it's redundant)
  names(config) <- names(model_row)
  config$model_id <- NULL

  # Remove empty values
  config <- config |>
    purrr::compact() |>
    purrr::discard(is.na) |>
    purrr::discard(~ .x == "")

  config
}

#' Get API key from environment variables
#'
#' @param provider The provider name from models.csv.
#'
#' @return The API key for the provider, or NULL if no key is needed.
#'
#' @keywords internal
get_api_key <- function(provider) {
  # Skip key for providers that don't need one
  if (provider == "ollama") {
    return(NULL)
  }

  # Build environment variable name
  env_var <- paste0(toupper(provider), "_API_KEY")

  # Get the key from environment
  key <- get_env_var(env_var)
  if (identical(key, "")) {
    cli::cli_abort(
      "Can't find env var {.code {env_var}} for provider {.code {provider}}"
    )
  }

  key
}

#' Get environment variable from package root .env file
#'
#' @param var_name The name of the environment variable to retrieve.
#' @return The value of the environment variable, or NULL if not found.
#'
#' @keywords internal
get_env_var <- function(var_name) {
  # Get package root directory
  if (file.exists("DESCRIPTION")) {
    root_dir <- "."
  } else {
    # Try to find package root by looking for DESCRIPTION file
    paths <- strsplit(getwd(), .Platform$file.sep)[[1]]
    found <- FALSE

    for (i in rev(seq_along(paths))) {
      test_path <- do.call(file.path, as.list(paths[1:i]))
      if (file.exists(file.path(test_path, "DESCRIPTION"))) {
        found <- TRUE
        test_path
        break
      }
    }

    if (!found) {
      cli::cli_abort("Could not find package root directory")
    }

    root_dir <- test_path
  }

  # Read .env file from package root
  env_file <- file.path(root_dir, ".env")
  if (!file.exists(env_file)) {
    cli::cli_abort("Could not find .env file in package root")
  }
  readRenviron(env_file)

  # Get environment variable
  value <- Sys.getenv(var_name)
  if (identical(value, "")) {
    cli::cli_abort(
      "Environment variable {.code {var_name}} not found in .env file"
    )
  }

  value
}

#' Sequence Generation from Vector Range
#'
#' Extends seq() to automatically compute from and to from the vector range
#' values. This function takes a vector, finds its range (min and max), and
#' generates a sequence between those values.
#'
#' @param x A numeric vector from which to extract the range.
#' @param by Number: increment of the sequence. If NULL, defaults to 1 for
#'   integer sequences.
#' @param length.out Desired length of the sequence. A non-negative number,
#'   which for seq() will be rounded up if fractional.
#' @param along.with Take the length from the length of this argument.
#' @param ... Further arguments passed to seq().
#'
#' @return A numeric vector containing the sequence from min(x) to max(x).
#'
#' @examples
#' \dontrun{
#' # Basic usage with by parameter
#' seq_range(c(1, 10), by = 1)
#'
#' # Using length.out instead of by
#' seq_range(c(0, 100), length.out = 11)
#'
#' # Works with unordered input
#' seq_range(c(10, 1, 5, 3), by = 2)
#' }
#'
seq_range <- function(
  x,
  by = NULL,
  length.out = NULL,
  along.with = NULL,
  ...
) {
  x <- range(x)
  from <- x[1]
  to <- x[2]

  # Build arguments list, excluding NULL values
  args <- list(from = from, to = to)

  if (!is.null(by)) args$by <- by
  if (!is.null(length.out)) args$length.out <- length.out
  if (!is.null(along.with)) args$along.with <- along.with

  # Add any additional arguments from ...
  extra_args <- list(...)
  args <- c(args, extra_args)

  do.call(seq, args)
}
