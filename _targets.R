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
  "scales",
  "ggrepel",
  "gt"
)

tar_option_set(
  workspace_on_error = TRUE,
  packages = pkgs,
  controller = crew::crew_controller_local(
    workers = parallel::detectCores(),
    garbage_collection = TRUE,
    options_local = crew::crew_options_local("logs"),
    seconds_idle = 60 # Clean up idle workers after 60 seconds
  ),
  error = "null", # Keep going even if error. We will review all errors later
  debug = NULL # put here targets to investigate, as a string or vector
)

# Load functions
tar_source("R")

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
    }
  ),

  tar_target(
    processing_dir,
    {
      path <- here::here("results", "processing")
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      path
    }
  ),

  tar_target(
    models_dir,
    {
      path <- here::here("models")
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      path
    }
  ),

  tar_target(
    outputs_dir,
    {
      path <- here::here("outputs")
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      path
    }
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

  # Model fitting ----------

  # MCMC configuration parameters
  tar_target(
    mcmc_config,
    {
      detected_cores <- parallel::detectCores()
      cores <- min(2L, detected_cores)
      threads <- floor(detected_cores / cores)

      list(
        backend = "cmdstanr",
        chains = 2L,
        iter = 4000L,
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
      priors <- c(
        # LKJ prior for correlation matrices
        brms::prior(lkj(2), class = "cor"),
        # Random-effect SDs for item intercepts
        brms::prior(student_t(3, 0, 1.5), class = "sd")
      )

      file_path <- file.path(models_dir, "fit_correctness.rds")

      message("Fitting correctness model...")

      brms::brm(
        formula = correct | trials(total) ~
          0 + (modality | model_id) + (1 | item),
        family = binomial(),
        data = data_correctness,
        prior = priors,
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
      priors <- c(
        # LKJ prior for correlation matrices
        brms::prior(lkj(2), class = "cor"),
        # Random-effect SDs for item intercepts
        brms::prior(
          student_t(3, 0, 1.5),
          class = "sd",
          dpar = "muclean"
        ),
        brms::prior(
          student_t(3, 0, 1.5),
          class = "sd",
          dpar = "murescued"
        )
      )

      file_path <- file.path(models_dir, "fit_parsing.rds")

      message("Fitting parsing model...")

      brms::brm(
        formula = y | trials(total) ~ 0 + (modality | model_id) + (1 | item),
        family = multinomial(),
        data = data_parsing,
        prior = priors,
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
      priors <- c(
        # LKJ prior for correlation matrices
        brms::prior(lkj(2), class = "cor"),
        # Random-effect SDs for item intercepts
        brms::prior(
          student_t(3, 0, 1.5),
          class = "sd",
          dpar = "muB"
        ),
        brms::prior(
          student_t(3, 0, 1.5),
          class = "sd",
          dpar = "muC"
        ),
        brms::prior(
          student_t(3, 0, 1.5),
          class = "sd",
          dpar = "muD"
        )
      )

      file_path <- file.path(models_dir, "fit_consistency.rds")

      message("Fitting consistency model...")

      brms::brm(
        formula = y | trials(total) ~ 0 + (modality | model_id) + (1 | item),
        family = multinomial(),
        data = data_consistency,
        prior = priors,
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

  # Posterior Analysis ----------

  # Number of posterior draws to extract (NULL = all draws)
  tar_target(
    posterior_draws,
    NULL
  ),

  # Extract posterior draws for all models
  tar_target(
    draws_correctness,
    {
      options(mc.cores = parallel::detectCores())
      extract_posterior_draws(
        model = readRDS(fit_correctness),
        ndraws = posterior_draws,
        seed = mcmc_config$seed
      )
    }
  ),

  tar_target(
    draws_parsing,
    {
      options(mc.cores = parallel::detectCores())
      extract_posterior_draws(
        model = readRDS(fit_parsing),
        ndraws = posterior_draws,
        seed = mcmc_config$seed
      )
    }
  ),

  tar_target(
    draws_consistency,
    {
      options(mc.cores = parallel::detectCores())

      # Extract raw posterior draws
      extract_posterior_draws(
        model = readRDS(fit_consistency),
        ndraws = posterior_draws,
        seed = mcmc_config$seed
      )
    }
  ),

  # Per-question consistency scores
  tar_target(
    consistency_kl_draws,
    compute_consistency_kl(draws_consistency)
  ),

  tar_target(
    consistency_simpson_draws,
    compute_consistency_simpson(draws_consistency)
  ),

  tar_target(
    consistency_modal_draws,
    compute_consistency_modal(draws_consistency)
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

  # KL Divergence Summaries
  tar_target(
    summaries_consistency_kl_by_model,
    compute_marginalized_summaries(
      draws = consistency_kl_draws,
      group_vars = "model_id"
    )
  ),
  tar_target(
    summaries_consistency_kl_by_modality,
    compute_marginalized_summaries(
      draws = consistency_kl_draws,
      group_vars = "modality"
    )
  ),
  tar_target(
    summaries_consistency_kl_interaction,
    compute_marginalized_summaries(
      draws = consistency_kl_draws,
      group_vars = c("model_id", "modality")
    )
  ),

  # Simpson Index Summaries
  tar_target(
    summaries_consistency_simpson_by_model,
    compute_marginalized_summaries(
      draws = consistency_simpson_draws,
      group_vars = "model_id"
    )
  ),
  tar_target(
    summaries_consistency_simpson_by_modality,
    compute_marginalized_summaries(
      draws = consistency_simpson_draws,
      group_vars = "modality"
    )
  ),
  tar_target(
    summaries_consistency_simpson_interaction,
    compute_marginalized_summaries(
      draws = consistency_simpson_draws,
      group_vars = c("model_id", "modality")
    )
  ),

  # Modal Probability Summaries
  tar_target(
    summaries_consistency_modal_by_model,
    compute_marginalized_summaries(
      draws = consistency_modal_draws,
      group_vars = "model_id"
    )
  ),
  tar_target(
    summaries_consistency_modal_by_modality,
    compute_marginalized_summaries(
      draws = consistency_modal_draws,
      group_vars = "modality"
    )
  ),
  tar_target(
    summaries_consistency_modal_interaction,
    compute_marginalized_summaries(
      draws = consistency_modal_draws,
      group_vars = c("model_id", "modality")
    )
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

  # Plotting ----------

  # Generate and store plots
  tar_target(
    correctness_plots,
    plot_summaries(
      summaries = summaries_correctness_interaction,
      metric_name = "correctness",
      y_transform = scales::percent_format(accuracy = 1),
      y_label = "Correctness probability"
    )
  ),

  tar_target(
    parsing_plots,
    plot_summaries(
      summaries = summaries_parsing_interaction,
      metric_name = "parsing quality",
      y_label = "Parsing quality (ordinal scale)"
    )
  ),

  tar_target(
    consistency_kl_plots,
    plot_summaries(
      summaries = summaries_consistency_kl_interaction,
      metric_name = "KL consistency",
      y_label = "Consistency (KL Divergence)"
    )
  ),

  tar_target(
    consistency_simpson_plots,
    plot_summaries(
      summaries = summaries_consistency_simpson_interaction,
      metric_name = "Simpson consistency",
      y_label = "Consistency (Simpson Index)"
    )
  ),

  tar_target(
    consistency_modal_plots,
    plot_summaries(
      summaries = summaries_consistency_modal_interaction,
      metric_name = "Modal consistency",
      y_label = "Consistency (Modal Probability)"
    )
  ),

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
    correlation_correctness_consistency_kl,
    compute_model_correlation(
      model1_draws = draws_correctness,
      model2_draws = consistency_kl_draws,
      filter_parsing = FALSE
    )
  ),

  tar_target(
    correlation_correctness_consistency_simpson,
    compute_model_correlation(
      model1_draws = draws_correctness,
      model2_draws = consistency_simpson_draws,
      filter_parsing = FALSE
    )
  ),

  tar_target(
    correlation_correctness_consistency_modal,
    compute_model_correlation(
      model1_draws = draws_correctness,
      model2_draws = consistency_modal_draws,
      filter_parsing = FALSE
    )
  ),

  # Pareto frontier analysis
  tar_target(
    pareto_frontier_plot,
    plot_pareto_frontier(
      correctness_summaries = summaries_correctness_by_model,
      models_data = models
    )
  ),

  # Tables ----------
  tar_target(
    table_correctness,
    create_summary_table(
      summaries = summaries_correctness_interaction,
      metric_name = ".prob",
      title = "Model Correctness",
      subtitle = "Probability of generating the correct answer",
      source_note = "Summaries are posterior medians with 95% CrIs.",
      group_by_var = "modality"
    )
  ),

  tar_target(
    table_parsing,
    create_summary_table(
      summaries = summaries_parsing_interaction,
      metric_name = ".prob",
      title = "Clean Parsing Rate",
      subtitle = "Probability of responses being cleanly parsed (no fixes needed)",
      source_note = "Summaries are posterior medians with 95% CrIs.",
      group_by_var = "modality"
    )
  ),

  tar_target(
    table_consistency,
    create_summary_table(
      summaries = summaries_consistency_kl_interaction,
      metric_name = ".prob",
      title = "Response Consistency (Scaled KL Divergence)",
      subtitle = "Score of 1 is perfect consistency, 0 is uniform inconsistency",
      source_note = "Summaries are posterior medians with 95% CrIs.",
      group_by_var = "modality"
    )
  ),

  # Save tables to files
  tar_target(
    table_correctness_file,
    save_gt_table(
      table_correctness,
      path = here::here("outputs", "tables", "correctness.html")
    ),
    format = "file"
  ),

  tar_target(
    table_parsing_file,
    save_gt_table(
      table_parsing,
      path = here::here("outputs", "tables", "parsing.html")
    ),
    format = "file"
  ),

  tar_target(
    table_consistency_file,
    save_gt_table(
      table_consistency,
      path = here::here("outputs", "tables", "consistency.html")
    ),
    format = "file"
  )
)
