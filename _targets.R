# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

pkgs <- c(
  "crew",
  "dplyr",
  "rlang",
  "ggplot2")

pak::pak(pkgs)

tar_option_set(
  workspace_on_error = TRUE,
  packages = pkgs

  # Parallel processing
  , controller = crew::crew_controller_local(workers = parallel::detectCores())

  , error = "null" # Keep going even if error. We will review all errors later
  , debug = NULL # put here targets to investigate, as a string or vector
)

# Load functions
tar_source("R")

# Pipeline definition
list(
  # Settings ----------

  # Replications
  tar_target(
    n_replications,
    5L  # Number of times each combination should be tested
  ),

  tar_target(
    temperature,
    0.5
  ),

  tar_target(
    max_tokens,
    1200
  ),

  # File paths
  tar_target(
    questions_file,
    here::here("inst", "benchmark_travel_medicine.csv"),
    format = "file"
  ),
  tar_target(
    models_file,
    here::here("inst", "models.csv"),
    format = "file"
  ),
  tar_target(
    processed_dir,
    {
      path <- here::here("results", "processed")
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      path
    },
    format = "file"
  ),

  tar_target(
    processing_dir,
    {
      path <- here::here("results", "processing")
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      path
    },
    format = "file"
  ),

  # Define which status to reprocess
  tar_target(
    reprocess_status,
    c("E")  # E = ERROR
  ),

  tar_target(
    question_ids,
    1:42 # max is 42
  ),

  # Data Loading ----------

  # Parse and validate questions
  tar_target(
    questions,
    load_questions(questions_file)[question_ids, ]
  ),

  # Parse and validate models
  tar_target(
    models,
    load_models(models_file)
  ),

  # Experiment Setup ----------

  # Define query modalities
  tar_target(
    modalities,
    c("cold", "free", "reasoning")
  ),

  # Create combinations of questions, models, and modalities
  tar_target(
    combinations,
    {
      # Create all possible combinations
      all_combinations <- expand.grid(
        item = questions$item,
        model_id = models$model_id[models$active == 1],
        modality = modalities,
        replication = seq_len(n_replications),
        stringsAsFactors = FALSE
      ) |>
        # Add question details
        left_join(
          questions |> select("item", "item_text", "option_correct"),
          by = "item") |>
        # Exclude combinations in which a reasoning prompt is used for a
        # reasoning model, since it's redundant (and openai forbids them)
        left_join(
          models |> select("model_id", "model_type"),
          by = "model_id"
        ) |>
        filter(
          !(.data$modality == "reasoning" & .data$model_type == "reasoning")) |>
        select(-"model_type")


      # Get list of existing result files
      existing_files <- list.files(
        processed_dir,
        pattern = "^[CFNE]\\..+\\.csv$",
        full.names = FALSE
      ) |>
        # Keep only files that don't need reprocessing
        stringr::str_subset(
          paste0("^[", paste0(reprocess_status, collapse = ""), "]\\."),
          negate = TRUE
        ) |>
        stringr::str_remove("^\\w.")

      # Generate file names for all combinations
      prospective_files <- generate_temp_filename(
        data = all_combinations |> mutate(status = "T"), # Dummy status
        dir = processed_dir
      ) |>
        basename() |>
        stringr::str_remove("^\\w.")

      # Filter in unprocessed combinations
      to_test <- all_combinations[!prospective_files %in% existing_files, ]

      if (nrow(to_test) == 0) stop(
        "All combinations were already processed.",
        call. = F)

      to_test
    },
    error = "abridge"
  ),

  # LLM Querying ----------

  # Query LLMs and save results incrementally
  tar_target(
    processed_files,
    {
      question <- get_formatted_question(combinations$item, questions)
      model_config <- get_model_config(combinations$model_id)
      model_config$system_prompt <- generate_system_prompt(
        combinations$modality)

      cli::cli_inform(
        paste(
          "Running item: {combinations$item}, {combinations$model_id}, {combinations$modality}, repl. {combinations$replication}")
        )

      # Set some parameters for all models
      model_config$temperature <- temperature

      str(model_config)

      # Create a temp file to track what combination is being processed
      temp_file <- combinations[1,] |> mutate(status = "T") |>
        generate_temp_filename()

      readr::write_csv(
        data.frame(combinations, model_config),
        file.path(processing_dir, temp_file)
      )

      on.exit({
        file.remove(file.path(processing_dir, temp_file))
      })

      # Try to get LLM response
      response <- tryCatch({
        query_llm(
          message = question,
          model_config = model_config
        )
      }, error = function(e) {
        # Return error message plus metadata
        list(
          answer = sprintf(
            "ERROR: %s", stringr::str_replace_all(e$message, "[\n\r]", " ")),
          metadata = list(
            timestamp = Sys.time(),
            generation_time = NA_real_,
            input_tokens = NA_integer_,
            output_tokens = NA_integer_,
            total_tokens = NA_integer_
          )
        )
      })

      # Process response and determine status
      processed <- extract_answer(response$answer)
      status <- determine_status(processed, combinations$option_correct)

      # Create result row
      new_result <- tibble(
        combinations,
        raw_response = response$answer,
        answer = processed,
        status = status,
        response$metadata |> as.data.frame()
      )

      # Remove the results connected to this combination if any
      prospective_file_name <- generate_temp_filename(new_result) |>
        stringr::str_remove("^\\w\\.")

      list.files(
        processed_dir,
        pattern = paste0("\\.", prospective_file_name),
        full.names = TRUE) |>
        file.remove()

      if (status %in% reprocess_status) {
        store_temp_result(new_result, dir = processing_dir)
      }

      # Store result
      store_temp_result(new_result, dir = processed_dir)
    },
    pattern = map(combinations),
    format = "file"
  ),

  # Results ----------

  # Load results into a data frame
  tar_target(
    results,
    {
      processed_files; # add dependency

      report_status_distribution() |> print()

      compile_results_data(dir = processed_dir)

    }
  ),

  tar_target(
    result_file,
    {
      file <- here::here("results", "all_results.csv")
      readr::write_csv(results, file)
      file
    },
    format = "file"
  )
)
