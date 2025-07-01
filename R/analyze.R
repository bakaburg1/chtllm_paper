#' Prepare data for correctness model (binomial)
#'
#' Filters and aggregates results data to create a dataset suitable for the
#' binomial correctness model. Excludes error and not-found responses, then
#' summarizes correct and total responses by item, model, and modality.
#'
#' @param results Raw results data from targets pipeline.
#'
#' @return A tibble with correct/total counts by item/model_id/modality.
#'
#' @export
prepare_correctness_data <- function(results) {
  results |>
    filter(!.data$status %in% c("E", "N")) |>
    summarise(
      correct = sum(.data$status == "C"),
      total = n(),
      .by = c("item", "model_id", "modality")
    ) |>
    filter(.data$total > 0)
}

#' Prepare data for parsing model (ordinal)
#'
#' Converts each LLM response to an ordered parsing‐quality outcome suitable for
#' an **ordered logit** model. Responses are classified as:
#'
#' * `none`    - the answer could **not** be parsed (`status == "N"`).
#' * `rescued` - the answer required a fix (an asterisk `*` detected).
#' * `clean`   - the answer was parsed without changes.
#'
#' The returned data contains **one row per response** with an ordered factor
#' `y` (levels `none < rescued < clean`) plus the grouping variables used in the
#' model (`item`, `model_id`, `modality`).
#'
#' @param results Raw results data from the targets pipeline.
#'
#' @return A tibble with columns `item`, `model_id`, `modality`, and the ordered
#'   response variable `y`.
#'
#' @export
prepare_parsing_data <- function(results) {
  # Map each result to an ordered parsing quality factor (none < rescued <
  # clean).

  # This produces one row per LLM response, which is required for fitting an
  # ordered‐logit model in **brms**. The returned tibble contains the ordered
  # outcome `y` and the predictors used in the model.

  # Define the ordered levels once to avoid repetition
  levels_ordered <- c("none", "rescued", "clean")

  results |>
    dplyr::filter(!.data$status %in% "E") |>
    dplyr::mutate(
      parse_category = dplyr::case_when(
        .data$status == "N" ~ "none",
        stringr::str_detect(.data$answer, "\\*") ~ "rescued",
        .default = "clean"
      ),
      # Convert to an **ordered** factor so that brms treats it as ordinal.
      y = factor(parse_category, levels = levels_ordered, ordered = TRUE)
    ) |>
    dplyr::select("item", "model_id", "modality", "y")
}

#' Prepare data for consistency model (binomial)
#'
#' Aggregates results to compute, for each (item, model, modality) cell, the
#' number of *distinct* answers produced (`y`) out of the total number of
#' responses (`total`). A value of `y = 1` indicates perfect consistency (the
#' model always produced the same answer), while larger values indicate lower
#' consistency. Error (`"E"`) and not-found (`"N"`) records are excluded.
#'
#' @param results Raw results data from targets pipeline.
#'
#' @return A tibble with columns `item`, `model_id`, `modality`, `y`, and
#'   `total`, suitable for fitting a binomial model via `y | trials(total)`.
#'
#' @export
prepare_consistency_data <- function(results) {
  results |>
    dplyr::filter(!.data$status %in% c("E", "N")) |>
    dplyr::mutate(
      answer = stringr::str_remove_all(.data$answer, "\\*")
    ) |>
    dplyr::summarise(
      y = dplyr::n_distinct(.data$answer),
      total = dplyr::n(),
      .by = c("item", "model_id", "modality")
    ) |>
    dplyr::filter(.data$total > 0)
}

#' Extract posterior draws from fitted Bayesian models
#'
#' Extracts posterior draws from fitted brms models using tidybayes, with
#' support for subsampling and reproducible results.
#'
#' This function provides a consistent interface for extracting posterior
#' predictive draws across different brms model types (binomial, multinomial,
#' ordinal). When ndraws is specified and is less than the total number of
#' posterior draws, the function will randomly subsample. Use the seed parameter
#' to ensure reproducible subsampling.
#'
#' @param model Fitted brms model object.
#' @param ndraws Number of draws to extract (NULL for all draws).
#' @param resp Response variable name for multivariate models (NULL for
#'   univariate).
#' @param seed Random seed for reproducible subsampling when ndraws is not NULL.
#'   If NULL, results may vary between runs.
#'
#' @return A tibble with posterior draws, always using `.prob` as the value
#'   column for consistency across model types.
#'
#' @export
extract_posterior_draws <- function(
  model,
  ndraws = NULL,
  resp = NULL,
  seed = NULL
) {
  # Prepare newdata based on model type
  newdata <- model$data |>
    select("item", "model_id", "modality", any_of(c("total"))) |>
    distinct() |>
    mutate(total = 1)

  # Extract draws with appropriate parameters
  args <- list(
    object = model,
    newdata = newdata,
    allow_new_levels = FALSE
  )

  if (!is.null(ndraws)) {
    args$ndraws <- ndraws
  }

  if (!is.null(resp)) {
    args$resp <- resp
  }

  # Add seed parameter for reproducible subsampling
  if (!is.null(seed)) {
    args$seed <- seed
  }

  # Always use .prob as value name
  args$value <- ".prob"

  # Extract draws
  do.call(tidybayes::add_epred_draws, args) |>
    # Remove grouping structure
    ungroup()
}

#' Compute marginalized posterior summaries
#'
#' Computes posterior summaries after marginalizing over specified grouping
#' variables. Supports different marginalization strategies for various analysis
#' needs.
#'
#' @param draws Output from extract_posterior_draws().
#' @param marginalize_over Vector of variable names to marginalize over; the
#'   posterior mean will be computed for each draw and each unique combination
#'   of these variables. Defaults to `"model_id"`.
#' @param summary_vars Variables to summarize (default: .prob).
#' @param ci_width Credible interval width (default: 0.95).
#'
#' @return A tibble with posterior summaries.
#'
#' @export
compute_marginalized_summaries <- function(
  draws,
  marginalize_over = "model_id",
  summary_vars = ".prob",
  ci_width = 0.95
) {
  rlang::check_installed("tidybayes")

  # Check that the summary variable is present in the draws
  if (!summary_vars %in% names(draws)) {
    stop("Could not find .prob column in draws")
  }

  # Special handling for parsing data: focus on "clean" category only
  if (".category" %in% names(draws) && "clean" %in% unique(draws$.category)) {
    draws <- draws |>
      filter(.data$.category == "clean")
  }

  # When the caller asks to marginalise over `model_type` we need to make sure
  # that such a column exists in `draws`. If it is missing we try to retrieve
  # it from `load_models()` and join by `model_id`.
  if ("modality" %in% marginalize_over && !("model_type" %in% names(draws))) {
    models_df <- tryCatch(
      load_models(),
      error = \(e) NULL
    )

    if (!is.null(models_df)) {
      draws <- dplyr::left_join(
        draws,
        models_df |>
          dplyr::select("model_id", "model_type"),
        by = "model_id"
      )
    } else {
      cli::cli_warn(
        "Requested marginalisation over {.val model_type}, but model metadata could not be loaded."
      )
    }
  }

  # Group and summarize
  draws |>
    summarise(
      # Marginalizing step
      across(all_of(summary_vars), mean),
      .by = all_of(c(marginalize_over, ".draw"))
    ) |>
    group_by(pick(all_of(marginalize_over))) |>
    tidybayes::median_qi(!!!syms(summary_vars), .width = ci_width) |>
    ungroup()
}

#' Compute posterior correlation with flexible marginalisation
#'
#' Provides a unified engine for computing Spearman correlations between two
#' sets of posterior draws, with optional marginalisation (averaging) over any
#' combination of grouping variables - e.g. `"model_id"` (default),
#' `c("model_id", "modality")`, or `c("item", "model_id", "modality")`.
#'
#' For every posterior draw the function first collapses the probability
#' (`.prob`) by the variables supplied in `marginalize_over`. A correlation is
#' then computed across the resulting rows (i.e. across the unique combinations
#' of those variables). Finally these per-draw correlations are summarised with
#' `tidybayes::median_qi()`.
#'
#' @param model1_draws,model2_draws Posterior draws for the two metrics to be
#'   correlated. Must contain `.prob`, `.draw`, and the columns listed in
#'   `marginalize_over`.
#' @param marginalize_over Character vector of variables to average over before
#'   computing the correlation. Defaults to `"model_id"` (i.e. between-model
#'   correlation).
#'
#' @return A tibble with the posterior median correlation and its credible
#'   interval.
#'
#' @export
compute_marginalized_correlation <- function(
  model1_draws,
  model2_draws,
  marginalize_over = "model_id"
) {
  # Optionally keep only clean parsing rows
  keep_clean <- function(draws) {
    if (".category" %in% names(draws)) {
      draws |> dplyr::filter(.data$.category == "clean")
    } else {
      draws
    }
  }

  model1_draws <- keep_clean(model1_draws)
  model2_draws <- keep_clean(model2_draws)

  # Helper to perform marginalisation
  summarise_draws <- function(draws) {
    if (is.null(marginalize_over) || length(marginalize_over) == 0) {
      return(draws)
    }

    draws |>
      dplyr::summarise(
        .prob = mean(.data$.prob),
        .by = dplyr::all_of(c(marginalize_over, ".draw"))
      )
  }

  d1 <- summarise_draws(model1_draws) |>
    dplyr::rename(prob1 = .data$.prob)
  d2 <- summarise_draws(model2_draws) |>
    dplyr::rename(prob2 = .data$.prob)

  # Determine join variables
  join_vars <- intersect(names(d1), names(d2))
  join_vars <- setdiff(join_vars, c("prob1", "prob2"))

  cor_by_draw <- d1 |>
    dplyr::inner_join(d2, by = join_vars) |>
    dplyr::summarise(
      correlation = cor(
        .data$prob1,
        .data$prob2,
        use = "complete.obs",
        method = "spearman"
      ),
      .by = ".draw"
    )

  cor_by_draw |>
    tidybayes::median_qi(.data$correlation)
}

#' Compute KL Divergence consistency score from posterior draws
#'
#' Transforms posterior probability draws into a scaled KL divergence value,
#' measuring deviation from a uniform distribution. A score of 1 represents
#' perfect consistency (a deterministic response), while a score of 0 represents
#' perfect inconsistency (a uniform distribution of responses).
#'
#' @param draws Posterior draws from multinomial consistency model with .prob
#'   and .category columns.
#'
#' @return A tibble with scaled KL divergence values in .prob column. Higher
#'   values indicate higher consistency.
#'
#' @export
compute_consistency_kl <- function(draws) {
  # Determine number of options from the data
  n_options <- length(unique(draws$.category))
  uniform_prob <- 1 / n_options
  log_n_options <- log(n_options)

  draws |>
    ungroup() |>
    summarise(
      # Calculate KL divergence from uniform distribution
      kl_raw = sum(.data$.prob * log(.data$.prob / uniform_prob + 1e-10)),
      # Scale to [0, 1] range. 1 = consistent, 0 = inconsistent.
      .prob = 1 - (.data$kl_raw / log_n_options),
      .by = c("model_id", "modality", "item", ".draw")
    ) |>
    # Use .prob column for seamless pipeline integration
    select("model_id", "modality", "item", ".draw", ".prob")
}

#' Compute Simpson consistency score from posterior draws
#'
#' Calculates the Simpson concentration index from posterior probability draws.
#' The index represents the probability that two randomly selected responses
#' would be the same. A score of 1 represents perfect consistency (a
#' deterministic response).
#'
#' @param draws Posterior draws from multinomial consistency model with .prob
#'   and .category columns.
#'
#' @return A tibble with Simpson index values in .prob column. Higher values
#'   indicate higher consistency.
#'
#' @export
compute_consistency_simpson <- function(draws) {
  draws |>
    ungroup() |>
    summarise(
      .prob = sum(.data$.prob^2),
      .by = c("model_id", "modality", "item", ".draw")
    ) |>
    # Use .prob column for seamless pipeline integration
    select("model_id", "modality", "item", ".draw", ".prob")
}

#' Compute Modal consistency score from posterior draws
#'
#' Calculates the modal probability from posterior draws. The score is the
#' probability of the most likely response category. A score of 1 represents
#' perfect consistency (a deterministic response).
#'
#' @param draws Posterior draws from multinomial consistency model with .prob
#'   and .category columns.
#'
#' @return A tibble with modal probability values in .prob column. Higher values
#'   indicate higher consistency.
#'
#' @export
compute_consistency_modal <- function(draws) {
  draws |>
    ungroup() |>
    summarise(
      .prob = max(.data$.prob),
      .by = c("model_id", "modality", "item", ".draw")
    ) |>
    # Use .prob column for seamless pipeline integration
    select("model_id", "modality", "item", ".draw", ".prob")
}
