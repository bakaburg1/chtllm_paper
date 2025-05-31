# Load packages required to define the pipeline:
library(targets)

# Packages whose functions are probably not namespaced
pkgs <- c(
  "dplyr",
  "ggplot2",
  "rlang",
  "stringr",
  "brms",
  "cmdstanr",
  "tidybayes",
  "forcats",
  "scales"
)

tar_option_set(
  workspace_on_error = TRUE,
  packages = pkgs,
  # Parallel processing
  controller = crew::crew_controller_local(workers = parallel::detectCores()),
  error = "null", # Keep going even if error. We will review all errors later
  debug = NULL # put here targets to investigate, as a string or vector
)

# Load functions
tar_source("R")

detected_cores <- parallel::detectCores()

# Pipeline definition
list(
  # Settings ----------

  # Replications
  tar_target(
    n_replications,
    5L # Number of times each combination should be tested
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
    c("E") # E = ERROR
  ),

  # Define which questions to process, 1:42 is all of them
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
          by = "item"
        ) |>
        # Add model details
        left_join(
          models |> select("model_id", "model_type"),
          by = "model_id"
        ) |>
        # Exclude combinations in which a reasoning prompt is used for a
        # reasoning model, since it's redundant (and openai forbids them)
        filter(
          !(.data$modality == "reasoning" & .data$model_type == "reasoning")
        ) |>
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

      # If no combinations left, return a minimal placeholder tibble
      # This avoids pipeline errors on empty target outputs
      if (nrow(to_test) == 0) {
        cli::cli_alert_info("No new combinations to process.")
        tibble::tibble(.is_placeholder = TRUE)
      } else {
        # Return the actual combinations to test (without placeholder flag)
        to_test
      }
    }
  ),

  # LLM Querying ----------

  # Query LLMs and save results incrementally
  tar_target(
    processed_files,
    {
      # Skip placeholder branch without doing work
      if (".is_placeholder" %in% names(combinations)) {
        dummy_file <- file.path(processed_dir, ".__placeholder_skip__")
        if (!file.exists(dummy_file)) {
          dir.create(
            dirname(dummy_file),
            showWarnings = FALSE,
            recursive = TRUE
          )
          file.create(dummy_file)
        }
        return(dummy_file)
      }
      question <- get_formatted_question(combinations$item, questions)
      model_config <- get_model_config(combinations$model_id)
      model_config$system_prompt <- generate_system_prompt(
        combinations$modality
      )

      options(ellmer_timeout_s = 120)

      cli::cli_inform(
        paste(
          "Running item: {combinations$item}, {combinations$model_id},",
          "{combinations$modality}, repl. {combinations$replication}"
        )
      )

      # Set some parameters for all models
      model_config$temperature <- temperature

      str(model_config)

      # Create a temp file to track what combination is being processed
      temp_file <- combinations[1, ] |>
        mutate(status = "T") |>
        generate_temp_filename()

      readr::write_csv(
        data.frame(combinations, model_config),
        file.path(processing_dir, temp_file)
      )

      on.exit({
        file.remove(file.path(processing_dir, temp_file))
      })

      # Try to get LLM response
      response <- tryCatch(
        {
          query_llm(
            message = question,
            model_config = model_config
          )
        },
        error = function(e) {
          # Return error message plus metadata
          list(
            answer = sprintf(
              "ERROR: %s",
              stringr::str_replace_all(e$message, "[\n\r]", " ")
            ),
            metadata = list(
              timestamp = Sys.time(),
              generation_time = NA_real_,
              input_tokens = NA_integer_,
              output_tokens = NA_integer_,
              total_tokens = NA_integer_
            )
          )
        }
      )

      # Process response and determine status
      processed <- extract_answer(response$answer)
      status <- determine_status(processed, combinations$option_correct)

      cat("Reponse:\n", response$answer)
      print(paste("Status:", status))

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
        full.names = TRUE
      ) |>
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
      processed_files # add dependency

      cat("Status distribution for processing results:\n")
      report_status_distribution(type = "processing") |> print()

      cat("Status distribution for processed results:\n")
      report_status_distribution(type = "processed") |> print()

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
  ),

  # Bayesian Analysis ----------

  # MCMC configuration parameters
  tar_target(
    mcmc_config,
    {
      detected_cores <- detected_cores # Edit bas
      cores <- max(2L, detected_cores)
      threads <- max(1L, floor(detected_cores / cores))

      list(
        backend = "cmdstanr",
        chains = 2L,
        iter = 5000L,
        warmup = 1000L,
        cores = cores,
        threads = threads,
        seed = 123
      )
    }
  ),

  # Prepare data for the three models
  tar_target(
    data_correctness,
    prepare_correctness_data(results)
  ),

  tar_target(
    data_parsing,
    prepare_parsing_data(results)
  ),

  tar_target(
    data_consistency,
    prepare_consistency_data(results)
  ),

  # Fit the three Bayesian models
  tar_target(
    fit_correctness,
    {
      priors_correctness <- c(
        brms::prior(normal(0, 1.5), class = "b"),
        brms::prior(student_t(3, 0, 1.5), class = "sd")
      )

      file_path <- "models/fit_correctness.rds"

      brms::brm(
        formula = correct | trials(total) ~
          0 + (modality | model_id) + (1 | item),
        family = binomial(),
        data = data_correctness,
        prior = priors_correctness,
        backend = mcmc_config$backend,
        cores = mcmc_config$cores,
        chains = mcmc_config$chains,
        threads = brms::threading(mcmc_config$threads),
        file = file_path,
        iter = mcmc_config$iter,
        warmup = mcmc_config$warmup,
        seed = mcmc_config$seed
      )

      file_path
    },
    format = "file"
  ),

  tar_target(
    fit_parsing,
    {
      priors_parsing <- c(
        brms::prior(normal(0, 1.5), class = "b"),
        brms::prior(student_t(3, 0, 1.5), class = "sd")
      )

      file_path <- "models/fit_parsing.rds"

      brms::brm(
        formula = parse_ord ~ (modality | model_id) + (1 | item),
        family = cumulative("logit"),
        data = data_parsing,
        prior = priors_parsing,
        backend = mcmc_config$backend,
        cores = mcmc_config$cores,
        chains = mcmc_config$chains,
        threads = brms::threading(mcmc_config$threads),
        file = file_path,
        iter = mcmc_config$iter,
        warmup = mcmc_config$warmup,
        seed = mcmc_config$seed
      )

      file_path
    },
    format = "file"
  ),

  tar_target(
    fit_consistency,
    {
      priors_consistency <- c(
        # Population effects for every non-reference outcome
        brms::prior(normal(0, 1.5), class = "b", dpar = "muB"),
        brms::prior(normal(0, 1.5), class = "b", dpar = "muC"),
        brms::prior(normal(0, 1.5), class = "b", dpar = "muD"),
        # Random-effect SDs for item intercepts
        brms::prior(
          student_t(3, 0, 1.5),
          class = "sd",
          group = "item",
          dpar = "muB"
        ),
        brms::prior(
          student_t(3, 0, 1.5),
          class = "sd",
          group = "item",
          dpar = "muC"
        ),
        brms::prior(
          student_t(3, 0, 1.5),
          class = "sd",
          group = "item",
          dpar = "muD"
        )
      )

      file_path <- "models/fit_consistency.rds"

      brms::brm(
        formula = y | trials(total) ~ 0 + (modality | model_id) + (1 | item),
        family = multinomial(),
        data = data_consistency,
        prior = priors_consistency,
        backend = mcmc_config$backend,
        save_pars = brms::save_pars(all = TRUE),
        cores = mcmc_config$cores,
        chains = mcmc_config$chains,
        threads = brms::threading(mcmc_config$threads),
        file = file_path,
        iter = mcmc_config$iter,
        warmup = mcmc_config$warmup,
        seed = mcmc_config$seed
      )

      file_path
    },
    format = "file"
  ),

  # Posterior Analysis ----------

  # Extract posterior draws for all models
  tar_target(
    draws_correctness,
    extract_posterior_draws(
      model = readRDS(fit_correctness),
      cores = mcmc_config$cores
    )
  ),

  tar_target(
    draws_parsing,
    extract_posterior_draws(
      model = readRDS(fit_parsing),
      cores = mcmc_config$cores
    )
  ),

  tar_target(
    draws_consistency,
    extract_posterior_draws(
      model = readRDS(fit_consistency),
      cores = mcmc_config$cores
    )
  ),

  # Marginalized summaries for each model
  tar_target(
    summaries_correctness_by_model,
    compute_marginalized_summaries(
      draws = draws_correctness,
      group_vars = "model_id"
    )
  ),

  tar_target(
    summaries_correctness_by_modality,
    compute_marginalized_summaries(
      draws = draws_correctness,
      group_vars = "modality"
    )
  ),

  tar_target(
    summaries_correctness_interaction,
    compute_marginalized_summaries(
      draws = draws_correctness,
      group_vars = c("model_id", "modality")
    )
  ),

  tar_target(
    summaries_parsing_by_model,
    compute_marginalized_summaries(
      draws = draws_parsing,
      group_vars = "model_id"
    )
  ),

  tar_target(
    summaries_parsing_by_modality,
    compute_marginalized_summaries(
      draws = draws_parsing,
      group_vars = "modality"
    )
  ),

  tar_target(
    summaries_parsing_interaction,
    compute_marginalized_summaries(
      draws = draws_parsing,
      group_vars = c("model_id", "modality")
    )
  ),

  tar_target(
    summaries_consistency_by_model,
    compute_marginalized_summaries(
      draws = draws_consistency,
      group_vars = "model_id"
    )
  ),

  tar_target(
    summaries_consistency_by_modality,
    compute_marginalized_summaries(
      draws = draws_consistency,
      group_vars = "modality"
    )
  ),

  tar_target(
    summaries_consistency_interaction,
    compute_marginalized_summaries(
      draws = draws_consistency,
      group_vars = c("model_id", "modality")
    )
  ),

  # KL divergence for consistency
  tar_target(
    kl_divergence_consistency,
    calculate_scaled_kl_divergence(draws_consistency)
  ),

  # # Store summary tables
  # tar_target(
  #   correctness_tables_file,
  #   {
  #     file_path <- here::here("outputs", "correctness_summaries.rds")
  #     dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  #
  #     summaries <- list(
  #       by_model = summaries_correctness_by_model,
  #       by_modality = summaries_correctness_by_modality,
  #       interaction = summaries_correctness_interaction
  #     )
  #
  #     saveRDS(summaries, file_path)
  #     file_path
  #   },
  #   format = "file"
  # ),
  #
  # tar_target(
  #   parsing_tables_file,
  #   {
  #     file_path <- here::here("outputs", "parsing_summaries.rds")
  #     dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  #
  #     summaries <- list(
  #       by_model = summaries_parsing_by_model,
  #       by_modality = summaries_parsing_by_modality,
  #       interaction = summaries_parsing_interaction
  #     )
  #
  #     saveRDS(summaries, file_path)
  #     file_path
  #   },
  #   format = "file"
  # ),
  #
  # tar_target(
  #   consistency_tables_file,
  #   {
  #     file_path <- here::here("outputs", "consistency_summaries.rds")
  #     dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  #
  #     summaries <- list(
  #       by_model = summaries_consistency_by_model,
  #       by_modality = summaries_consistency_by_modality,
  #       interaction = summaries_consistency_interaction,
  #       kl_divergence = kl_divergence_consistency
  #     )
  #
  #     saveRDS(summaries, file_path)
  #     file_path
  #   },
  #   format = "file"
  # ),

  # Generate and store plots
  tar_target(
    correctness_plots,
    create_summary_plots(
      summaries = summaries_correctness_interaction,
      metric_name = "correctness",
      y_transform = scales::percent_format(accuracy = 1),
      y_label = "Correctness probability"
    )
  ),

  tar_target(
    parsing_plots,
    create_summary_plots(
      summaries = summaries_parsing_interaction,
      metric_name = "parsing quality",
      y_label = "Parsing quality (ordinal scale)"
    )
  ),

  tar_target(
    consistency_plots,
    create_summary_plots(
      summaries = summaries_consistency_interaction,
      metric_name = "response consistency",
      y_label = "Response probability"
    )
  ),

  tar_target(
    kl_divergence_plots,
    create_summary_plots(
      summaries = kl_divergence_consistency,
      metric_name = "KL divergence",
      y_label = "Scaled KL divergence"
    )
  ),

  # tar_target(
  #   correctness_plots_file,
  #   {
  #     file_path <- here::here("outputs", "correctness_plots.rds")
  #     dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  #     saveRDS(correctness_plots, file_path)
  #     file_path
  #   },
  #   format = "file"
  # ),
  #
  # tar_target(
  #   parsing_plots_file,
  #   {
  #     file_path <- here::here("outputs", "parsing_plots.rds")
  #     dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  #     saveRDS(parsing_plots, file_path)
  #     file_path
  #   },
  #   format = "file"
  # ),
  #
  # tar_target(
  #   consistency_plots_file,
  #   {
  #     file_path <- here::here("outputs", "consistency_plots.rds")
  #     dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  #     saveRDS(consistency_plots, file_path)
  #     file_path
  #   },
  #   format = "file"
  # ),
  #
  # tar_target(
  #   kl_plots_file,
  #   {
  #     file_path <- here::here("outputs", "kl_divergence_plots.rds")
  #     dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  #     saveRDS(kl_divergence_plots, file_path)
  #     file_path
  #   },
  #   format = "file"
  # ),

  # Correlation analyses
  tar_target(
    correlation_correctness_parsing,
    compute_model_correlation(
      model1_draws = draws_correctness,
      model2_draws = draws_parsing,
      filter_parsing = TRUE,
      parsing_data = data_parsing
    )
  ),

  tar_target(
    correlation_correctness_consistency,
    compute_model_correlation(
      model1_draws = draws_correctness,
      model2_draws = draws_consistency,
      filter_parsing = FALSE
    )
  )

  # tar_target(
  #   correlations_file,
  #   {
  #     file_path <- here::here("outputs", "model_correlations.rds")
  #     dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  #
  #     correlations <- list(
  #       correctness_parsing = correlation_correctness_parsing,
  #       correctness_consistency = correlation_correctness_consistency,
  #       parsing_consistency = correlation_parsing_consistency
  #     )
  #
  #     saveRDS(correlations, file_path)
  #     file_path
  #   },
  #   format = "file"
  # )
)
