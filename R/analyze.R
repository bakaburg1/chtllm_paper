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

#' Prepare data for parsing model (multinomial)
#'
#' Aggregates parsing quality levels (none, rescued, clean) by item, model, and
#' modality to create data suitable for multinomial parsing modeling. Classifies
#' responses as "none" (not found), "rescued" (required cleaning), or "clean"
#' (direct answer).
#'
#' @param results Raw results data from targets pipeline.
#'
#' @return A tibble with counts for each parsing quality level.
#'
#' @export
prepare_parsing_data <- function(results) {
  results |>
    filter(.data$status != "E") |>
    mutate(
      parse_category = case_when(
        .data$status == "N" ~ "none",
        stringr::str_detect(.data$answer, "\\*") ~ "rescued",
        .default = "clean"
      )
    ) |>
    summarise(
      none = NA_real_,
      rescued = NA_real_,
      clean = NA_real_,
      across(
        c("none", "rescued", "clean"),
        ~ sum(.data$parse_category == cur_column())
      ),
      total = n(),
      .by = c("item", "model_id", "modality")
    ) |>
    filter(.data$total > 0) |>
    mutate(
      y = cbind(
        none = .data$none,
        rescued = .data$rescued,
        clean = .data$clean
      )
    )
}

#' Prepare data for consistency model (multinomial)
#'
#' Aggregates response choices (A, B, C, D) by item, model, and modality to
#' create data suitable for multinomial consistency modeling. Excludes error and
#' not-found responses.
#'
#' @param results Raw results data from targets pipeline.
#'
#' @return A tibble with counts for each answer option (A, B, C, D).
#'
#' @export
prepare_consistency_data <- function(results) {
  results |>
    filter(!.data$status %in% c("E", "N")) |>
    mutate(
      answer_clean = stringr::str_remove_all(.data$answer, "\\*")
    ) |>
    summarise(
      A = sum(.data$answer_clean == "A"),
      B = sum(.data$answer_clean == "B"),
      C = sum(.data$answer_clean == "C"),
      D = sum(.data$answer_clean == "D"),
      total = n(),
      .by = c("item", "model_id", "modality")
    ) |>
    filter(.data$total > 0) |>
    mutate(
      y = cbind(
        A = .data$A,
        B = .data$B,
        C = .data$C,
        D = .data$D
      )
    )
}

#' Calculate consistency metrics from multinomial model
#'
#' Computes Shannon entropy and Simpson diversity metrics from a fitted
#' multinomial consistency model to quantify response variability across model
#' and modality combinations.
#'
#' @param model A fitted `brms` multinomial model.
#'
#' @return A tibble with entropy and Simpson diversity metrics.
#'
#' @importFrom tidybayes add_epred_draws median_qi
#'
#' @export
calculate_consistency_metrics <- function(model) {
  rlang::check_installed("tidybayes")

  # Get predicted probabilities for each response option
  ep <- model |>
    tidybayes::add_epred_draws(
      newdata = model$data |> mutate(total = 1),
      value = ".prob",
      ndraws = 100
    )

  # Calculate entropy (H) and Simpson diversity (S)
  ep |>
    ungroup() |>
    summarise(
      # Shannon entropy: higher = more diverse/inconsistent
      H = -sum(.data$.prob * log(.data$.prob + 1e-10)),
      # Simpson diversity: 1 - sum(p^2), higher = more diverse
      S = 1 - sum(.data$.prob^2),
      .by = c("model_id", "modality", "item", ".draw")
    ) |>
    # Average across items for each model/modality
    summarise(
      across(c("H", "S"), mean),
      .by = c(".draw", "model_id", "modality")
    ) |>
    # Get posterior summaries
    group_by(.data$model_id, .data$modality) |>
    tidybayes::median_qi(.data$H, .data$S) |>
    ungroup()
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
    select("item", "model_id", "modality") |>
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
  summary_vars = NULL,
  ci_width = 0.95
) {
  rlang::check_installed("tidybayes")

  # Determine summary variables if not specified
  if (is.null(summary_vars)) {
    if (".prob" %in% names(draws)) {
      summary_vars <- ".prob"
    } else {
      stop("Could not find .prob column in draws")
    }
  }

  # Special handling for parsing data: focus on "clean" category only
  if (".category" %in% names(draws) &&
      "clean" %in% unique(draws$.category)) {
    draws <- draws |>
      filter(.data$.category == "clean")
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
#' combination of grouping variables – e.g. `"model_id"` (default),
#' `c("model_id", "modality")`, or `c("item", "model_id", "modality")`.
#'
#' For every posterior draw the function first collapses the probability
#' (`.prob`) by the variables supplied in `marginalize_over`.  A correlation is
#' then computed across the resulting rows (i.e. across the unique
#' combinations of those variables).  Finally these per-draw correlations are
#' summarised with `tidybayes::median_qi()`.
#'
#' @param model1_draws,model2_draws Posterior draws for the two metrics to be
#'   correlated.  Must contain `.prob`, `.draw`, and the columns listed in
#'   `marginalize_over`.
#' @param marginalize_over Character vector of variables to average over before
#'   computing the correlation.  Defaults to `"model_id"` (i.e. between-model
#'   correlation).
#' @param filter_parsing Logical; if `TRUE` and either draw set contains a
#'   `.category` column, only the `"clean"` category is kept (handy when one of
#'   the metrics is parsing quality).
#'
#' @return A tibble with the posterior median correlation and its credible
#'   interval.
#'
#' @export
compute_marginalized_correlation <- function(
  model1_draws,
  model2_draws,
  marginalize_over = "model_id",
  filter_parsing   = FALSE
) {
  rlang::check_installed("tidybayes")

  # Optionally keep only clean parsing rows
  keep_clean <- function(draws) {
    if (".category" %in% names(draws)) {
      draws |> dplyr::filter(.data$.category == "clean")
    } else {
      draws
    }
  }

  if (filter_parsing) {
    model1_draws <- keep_clean(model1_draws)
    model2_draws <- keep_clean(model2_draws)
  }

  # Helper to perform marginalisation
  summarise_draws <- function(draws) {
    if (is.null(marginalize_over) || length(marginalize_over) == 0) {
      return(draws)
    }

    draws |>
      dplyr::summarise(.prob = mean(.data$.prob), .by = all_of(c(marginalize_over, ".draw")))
  }

  d1 <- summarise_draws(model1_draws) |> rename(prob1 = .data$.prob)
  d2 <- summarise_draws(model2_draws) |> rename(prob2 = .data$.prob)

  # Determine join variables
  join_vars <- intersect(names(d1), names(d2))
  join_vars <- setdiff(join_vars, c("prob1", "prob2"))

  cor_by_draw <- d1 |>
    dplyr::inner_join(d2, by = join_vars) |>
    dplyr::summarise(
      correlation = cor(
        .data$prob1,
        .data$prob2,
        use    = "complete.obs",
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
