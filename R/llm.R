#' Generate system prompts for different modalities
#'
#' @param modality Character string indicating the modality ("cold", "free", or
#'   "reasoning").
#'
#' @return A character string containing the system prompt.
#'
#' @export
generate_system_prompt <- function(
  modality = c("cold", "free", "reasoning")
) {
  modality <- match.arg(modality)
  common_prompt <- "You are an AI expert in medical travel health answering multiple choice questions."

  prompt <- switch(
    modality,
    cold = paste(
      common_prompt,
      "You must ONLY respond with the letter of the correct answer",
      "in the format _X_ where X is A, B, C, or D.",
      "Do not provide ANY additional text or explanation."
    ),
    free = paste(
      common_prompt,
      "You can freely discuss and explain your thinking,",
      "but you MUST end your response with the letter of your",
      "chosen answer in the format _X_ where X is A, B, C, or D."
    ),
    reasoning = paste(
      common_prompt,
      "You must think carefully and methodically about each option.",
      "Explain your reasoning step by step, considering pros and cons.",
      "Question your assumptions and consider alternative viewpoints.",
      "After thorough analysis, provide your final answer",
      "in the format _X_ where X is A, B, C, or D.",
      "Do NOT use this format (_X_) in your reasoning, only for",
      "the final answer."
    ),
    stop("Invalid modality")
  )

  paste(
    prompt,
    "Be extra carefull in formatting the chosen options according to the _X_",
    "format, e.g. there should be only one _A_, _B_, _C_, or _D_.",
    "Avoid silly pitfalls like answering _X_ or incomplete answers like _A, B_,",
    "etc."
  )
}

#' Parse an unformatted answer using an LLM
#'
#' @param response The unformatted response to parse.
#'
#' @return A character string containing the extracted answer (A, B, C, or D) or
#'   NULL if parsing fails.
#'
#' @keywords internal
parse_answer_with_llm <- function(response) {
  # Get parser model config from environment
  parser_model_id <- get_env_var("PARSER_MODEL_ID")
  if (identical(parser_model_id, "")) {
    cli::cli_abort("Can't find env var {.code PARSER_MODEL_ID}")
  }

  # Load model configuration
  parser_config <- get_model_config(parser_model_id)

  # Create the parsing prompt
  parser_config$system_prompt <- "You are an AI assistant that examines the
  answers to a medicine closed-question test with only 4 options (A, B, C,
  or D). Your task is to extract the provided answer into a structured,
  parsable format. The format is the letter of the answer sorrounded by
  underscores, i.e., one of _A_, _B_, _C_, or _D_. You must not output any
  other text or explanation apart from the structured answer. If you cannot
  confidently identify the answer, respond only with _NONE_. Also answer
  _NONE_ if the reply seems incomplete." |>
    trimws()

  # Create a simple question format
  question <- paste(
    "Extract the intended answer (_A_, _B_, _C_, or _D_) from this text:\n",
    response
  )

  # Try to parse the answer
  result <- query_llm(
    message = question,
    model_config = parser_config
  )

  # Return NULL if parser says no answer found
  if (identical(result$answer, "NONE")) {
    return(NULL)
  }

  result$answer
}

#' Extract answer from LLM response
#'
#' @param response The LLM response text.
#'
#' @return A character string containing the extracted answer (A, B, C, or D).
#'
#' @export
extract_answer <- function(response) {
  if (grepl("ERROR|^Error", response)) {
    return("ERROR")
  }

  if (response %in% c("", NA)) {
    return("NONE")
  }

  if (stringr::str_detect(response, "_NONE_|(^NONE$)")) {
    return("NONE")
  }

  # Cold answers
  match <- stringr::str_match(response, "^_?([ABCD])_?$")[, 2]

  if (match %in% LETTERS[1:4]) {
    return(match)
  }

  # Pattern to match _X_ where X is A, B, C, or D
  pattern <- "_([ABCD])_"

  # Extract all matches
  match <- stringr::str_match_all(response, pattern)[[1]][, 2]

  if (length(match) > 0) {
    return(dplyr::last(match))
  }

  parsed_answer <- parse_answer_with_llm(response) |> extract_answer()

  parsed_answer <- paste0(parsed_answer, "*")

  return(parsed_answer)
}

#' Query an LLM with a message
#'
#' @param message The message to send to the LLM.
#' @param model_config List containing model configuration.
#'
#' @return A list containing the response content and metadata about the call.
#'
#' @export
query_llm <- function(
  message,
  model_config
) {
  # If model_config is a string, treat it as a model_id and load the config
  if (is.character(model_config) && length(model_config) == 1) {
    model_id <- model_config # Store the original model_id
    model_config <- get_model_config(model_config)
    model_config$model_id <- model_id # Add back model_id as character
  }

  # Add API key
  if (rlang::is_empty(model_config$api_key)) {
    model_config$api_key <- get_api_key(
      model_config$provider
    )
  }

  # Build the chat function name from provider
  chat_fn_str <- paste0("chat_", model_config$provider)

  # Get the function if overwritten in the package otherwise from ellmer
  chat_fn <- tryCatch(
    get(chat_fn_str),
    error = \(e) {
      get(chat_fn_str, envir = asNamespace("ellmer"))
    }
  )

  # Get function arguments
  fn_args <- names(formals(chat_fn))

  # Build the base parameters
  base_params <- model_config[fn_args] |> purrr::compact()
  base_params$echo <- "none"

  # Assign remaining parameters to model_params
  model_params <- model_config[!names(model_config) %in% names(base_params)]

  if (
    model_config$model_type == "reasoning" &&
      model_config$provider == "openrouter"
  ) {
    model_params$include_reasoning <- TRUE
  }

  # Remove model parameters that are not needed for the API call
  model_params$provider <- NULL
  model_params$model_type <- NULL
  model_params$active <- NULL

  retry <- TRUE
  retry_iter <- 1

  while (retry) {
    retry <- FALSE

    print(model_config$model)

    join_params <- c(base_params, list(api_args = model_params))

    # Create the chat object
    chat <- do.call(chat_fn, join_params)

    # Record start time
    start_time <- Sys.time()

    # Get response
    error <- try(response <- chat$chat(message), silent = TRUE)

    if (!inherits(error, "try-error")) {
      # Check errors
      http_resp <- httr2::last_response()
      if (http_resp |> httr2::resp_is_error()) {
        http_resp |> httr2::resp_body_string() |> rlang::inform()
        error <- http_resp |> httr2::resp_body_json() |> purrr::pluck(1)

        if (
          error$code %in%
            c(
              "unknown_parameter",
              "unsupported_parameter"
            )
        ) {
          retry <- TRUE
          model_params[error$param] <- NULL
        }
      }

      # Extract answer from response
      answer <- if (is.list(response) && !is.null(response$content)) {
        response$content
      } else {
        as.character(response)
      }

      # Trim whitespace
      answer <- stringr::str_trim(answer)

      # Calculate generation time
      end_time <- Sys.time()
      generation_time <- difftime(end_time, start_time)

      if (grepl("^Error", answer)) {
        browser()
      }

      # Get token counts for this call
      token_counts <- tryCatch(
        {
          tokens_matrix <- chat$tokens()

          # Get the last row which contains the current call's tokens
          last_row <- tokens_matrix[nrow(tokens_matrix), ]
          list(input = last_row["input"], output = last_row["output"])
        },
        error = function(e) {
          list(input = NA_integer_, output = NA_integer_)
        }
      )
    } else {
      answer <- as.character(error)
      token_counts <- list(input = NA, output = NA)
      generation_time <- NA
      retry <- FALSE

      cli::cli_alert_danger("Failed with error:\n{answer}")

      if (retry_iter <= 5) {
        wait <- exp(max(stats::rnorm(1, retry_iter, .05), 1))
        cli::cli_alert_warning("Retrying in {round(wait, 2)} seconds")
        retry_iter <- retry_iter + 1
        Sys.sleep(wait)
        retry <- TRUE
      }
    }
  }

  # Return all information
  list(
    # Core response
    answer = answer,

    # Metadata
    metadata = list(
      timestamp = start_time |> as.character(),
      generation_time = generation_time,
      input_tokens = token_counts$input,
      output_tokens = token_counts$output,
      total_tokens = sum(unlist(token_counts), na.rm = TRUE)
    ),

    # Input configuration
    config = list(
      model_id = model_config$model_id,
      provider = model_config$provider,
      model_name = model_config$model_name
    ) |>
      c(model_params),

    # Original inputs
    inputs = list(
      message = message
    )
  ) |>
    invisible()
}


#' Reimplementation of ellmer::chat_openai to support system messages with
#' o1-mini and ignore unsupported parameters (e.g. temperature) in all o-series
#' models
#'
#' @param system_prompt Character string for the system prompt.
#' @param turns A list of Turn objects representing the conversation history.
#' @param base_url The base URL for the OpenAI API.
#' @param api_key The API key for authentication.
#' @param model The specific model ID to use.
#' @param seed An integer seed for reproducibility.
#' @param api_args A list of additional arguments to pass to the API.
#' @param echo Logical; whether to echo the input prompts in the output.
chat_openai <- function(
  system_prompt = NULL,
  turns = NULL,
  base_url = "https://api.openai.com/v1",
  api_key = get_api_key("openai"),
  model = NULL,
  seed = NULL,
  api_args = list(),
  echo = FALSE
) {
  if (grepl("^o1-mini", model) && !is.null(system_prompt)) {
    turns <- list(ellmer::Turn(
      role = "user",
      contents = list(ellmer::ContentText(system_prompt))
    ))
    system_prompt <- NULL
  }

  # Temperature doesn't work with o-series models
  if (grepl("^o\\d+", model)) {
    api_args$temperature <- NULL # gives error if != 1
    api_args$n <- NULL # is ignored
    api_args$top_p <- NULL # unsupported
    api_args$presence_penalty <- NULL # unsupported
    api_args$frequency_penalty <- NULL # unsupported
  }

  ellmer::chat_openai(
    system_prompt = system_prompt,
    turns = turns,
    base_url = base_url,
    api_key = api_key,
    model = model,
    seed = seed,
    api_args = api_args,
    echo = echo
  )
}
