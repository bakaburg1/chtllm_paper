#' Prepare data for correctness model (binomial)
#'
#' Filters and aggregates results data to create a dataset suitable for the
#' binomial correctness model. Excludes error and not-found responses, then
#' summarizes correct and total responses by item, model, and modality.
#'
#' @param results Raw results data from targets pipeline.
#' @return A tibble with correct/total counts by item/model_id/modality.
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
#' Creates per-replication data with ordinal parsing quality levels based on
#' response patterns. Classifies responses as "none" (not found), "rescued"
#' (required cleaning), or "clean" (direct answer).
#'
#' @param results Raw results data from targets pipeline.
#' @return A tibble with per-replication ordinal parsing quality.
#' @export
prepare_parsing_data <- function(results) {
  results |>
    filter(.data$status != "E") |>
    mutate(
      parse_ord = factor(
        case_when(
          .data$status == "N" ~ "none",
          stringr::str_detect(.data$raw_response, "\\*") |
            (nchar(.data$raw_response) > 3 &
              .data$status %in% c("C", "F")) ~
            "rescued",
          stringr::str_detect(.data$raw_response, "^[ABCD]$") ~ "clean",
          .default = "rescued"
        ),
        levels = c("none", "rescued", "clean"),
        ordered = TRUE
      )
    ) |>
    select(
      "item",
      "model_id",
      "modality",
      "replication",
      "parse_ord"
    ) |>
    filter(!is.na(.data$parse_ord))
}

#' Prepare data for consistency model (multinomial)
#'
#' Aggregates response choices (A, B, C, D) by item, model, and modality to
#' create data suitable for multinomial consistency modeling. Excludes error
#' and not-found responses.
#'
#' @param results Raw results data from targets pipeline.
#' @return A tibble with counts for each answer option (A, B, C, D).
#' @export
prepare_consistency_data <- function(results) {
  results |>
    filter(!.data$status %in% c("E", "N")) |>
    mutate(
      answer_clean = stringr::str_remove(.data$answer, "\\*+")
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
    mutate(y = cbind(.data$A, .data$B, .data$C, .data$D))
}

#' Generate plots for model effects
#'
#' Creates conditional effects plots for fitted Bayesian models, including
#' model comparisons and modality effects visualizations.
#'
#' @param model A fitted `brms` model.
#' @param title_prefix Prefix for plot titles.
#' @return A list of `ggplot2` objects.
#' @importFrom brms conditional_effects
#' @export
plot_model_effects <- function(model, title_prefix = "Model") {
  rlang::check_installed(c("ggplot2", "forcats", "scales"))

  eff <- brms::conditional_effects(model, robust = TRUE)

  # Color palette for modalities
  pal <- c(
    cold = "lightblue",
    free = "darkgoldenrod1",
    reasoning = "darkred"
  )

  plots <- list()

  # Model effects plot
  if ("model_id" %in% names(eff)) {
    plots$model_effects <- eff$model_id |>
      mutate(
        model_id = forcats::fct_reorder(
          .data$model_id,
          .data$estimate__,
          .desc = FALSE
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::aes(.data$model_id, .data$estimate__) +
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$lower__, ymax = .data$upper__),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$estimate__),
        show.legend = FALSE,
        size = 3
      ) +
      ggplot2::scale_color_viridis_c() +
      ggplot2::coord_flip() +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::labs(
        x = NULL,
        title = paste(title_prefix, "by model"),
        subtitle = "Points = posterior median, bars = 95% CrI"
      )
  }

  # Modality effects plot
  if ("modality" %in% names(eff)) {
    plots$modality_effects <- eff$modality |>
      ggplot2::ggplot() +
      ggplot2::aes(.data$modality, .data$estimate__) +
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$lower__, ymax = .data$upper__),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$modality),
        size = 4
      ) +
      ggplot2::scale_colour_manual(values = pal, guide = "none") +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::labs(
        x = NULL,
        title = paste(
          "Effect of prompting modality on",
          tolower(title_prefix)
        ),
        subtitle = "Population-average predictions with 95% CrI"
      )
  }

  return(plots)
}

#' Calculate consistency metrics from multinomial model
#'
#' Computes Shannon entropy and Simpson diversity metrics from a fitted
#' multinomial consistency model to quantify response variability across
#' model and modality combinations.
#'
#' @param model A fitted `brms` multinomial model.
#' @return A tibble with entropy and Simpson diversity metrics.
#' @importFrom tidybayes add_epred_draws median_qi
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

#' Extract posterior draws for any brms model
#'
#' Extracts posterior draws using tidybayes::add_epred_draws with parallel
#' processing support. Works for binomial, ordinal, and multinomial models.
#'
#' @param model A fitted `brms` model.
#' @param cores Number of cores for parallel processing.
#' @param ndraws Number of posterior draws to extract.
#' @param resp Response variable name for multivariate models.
#'
#' @return A tibble with posterior draws.
#'
#' @importFrom tidybayes add_epred_draws
#'
#' @export
extract_posterior_draws <- function(
  model,
  cores = parallel::detectCores(),
  ndraws = NULL,
  resp = NULL
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
    cores = cores,
    allow_new_levels = FALSE
  )

  if (!is.null(ndraws)) {
    args$ndraws <- ndraws
  }

  if (!is.null(resp)) {
    args$resp <- resp
  }

  # For multinomial models, use .prob as value name
  if (
    inherits(model$family, "brmsfamily") && model$family$family == "multinomial"
  ) {
    args$value <- ".prob"
  }

  do.call(tidybayes::add_epred_draws, args)
}

#' Compute marginalized posterior summaries
#'
#' Computes posterior summaries after marginalizing over specified grouping
#' variables. Supports different marginalization strategies for various
#' analysis needs.
#'
#' @param draws Output from extract_posterior_draws().
#' @param group_vars Variables to group by for marginalization.
#' @param summary_vars Variables to summarize (default: .epred or .prob).
#' @param ci_width Credible interval width (default: 0.95).
#' @return A tibble with posterior summaries.
#' @importFrom tidybayes median_qi
#' @export
compute_marginalized_summaries <- function(
  draws,
  group_vars,
  summary_vars = NULL,
  ci_width = 0.95
) {
  rlang::check_installed("tidybayes")

  # Determine summary variables if not specified
  if (is.null(summary_vars)) {
    if (".epred" %in% names(draws)) {
      summary_vars <- ".epred"
    } else if (".prob" %in% names(draws)) {
      summary_vars <- ".prob"
    } else {
      stop("Could not determine summary variables automatically")
    }
  }

  # Group and summarize
  draws |>
    summarise(
      across(all_of(summary_vars), mean),
      .by = c(group_vars, ".draw")
    ) |>
    group_by(pick(all_of(group_vars))) |>
    tidybayes::median_qi(!!!syms(summary_vars), .width = ci_width) |>
    ungroup()
}

#' Calculate scaled KL divergence for consistency analysis
#'
#' Computes scaled Kullback-Leibler divergence from uniform distribution
#' for multinomial response patterns. Higher values indicate more
#' deterministic (less consistent) response patterns.
#'
#' @param draws Posterior draws from multinomial consistency model.
#' @param n_options Number of response options (default: 4 for A,B,C,D).
#' @return A tibble with scaled KL divergence metrics.
#' @export
calculate_scaled_kl_divergence <- function(draws, n_options = 4) {
  uniform_prob <- 1 / n_options
  log_n_options <- log(n_options)

  draws |>
    ungroup() |>
    summarise(
      # Calculate KL divergence from uniform distribution
      kl_raw = sum(.data$.prob * log(.data$.prob / uniform_prob + 1e-10)),
      # Scale to [0, 1] range
      kl_scaled = 1 - .data$kl_raw / log_n_options,
      .by = c("model_id", "modality", "item", ".draw")
    ) |>
    # Average across items for each model/modality
    summarise(
      across(c("kl_raw", "kl_scaled"), mean),
      .by = c(".draw", "model_id", "modality")
    ) |>
    # Get posterior summaries
    group_by(.data$model_id, .data$modality) |>
    tidybayes::median_qi(.data$kl_raw, .data$kl_scaled) |>
    ungroup()
}

#' Create comprehensive summary plots
#'
#' Generates publication-ready plots for marginalized posterior summaries,
#' including model comparisons, modality effects, and interaction plots.
#'
#' @param summaries Output from compute_marginalized_summaries().
#' @param metric_name Name of the metric being plotted.
#' @param y_transform Transformation function for y-axis (e.g.,
#'   scales::percent).
#' @param y_label Label for y-axis.
#'
#' @return A list of ggplot objects.
#'
#' @export
create_summary_plots <- function(
  summaries,
  metric_name,
  y_transform = NULL,
  y_label = NULL
) {
  rlang::check_installed(c("ggplot2", "forcats", "scales"))

  # Color palette for modalities
  pal <- c(
    cold = "lightblue",
    free = "darkgoldenrod1",
    reasoning = "darkred"
  )

  # Determine the main metric column
  metric_col <- names(summaries)[
    !names(summaries) %in%
      c(
        "model_id",
        "modality",
        ".lower",
        ".upper",
        ".width",
        ".point",
        ".interval"
      )
  ]

  if (length(metric_col) > 1) {
    metric_col <- metric_col[1]
    message("Multiple metric columns found, using: ", metric_col)
  }

  plots <- list()

  # Model comparison plot (averaged over modalities)
  if ("model_id" %in% names(summaries) && "modality" %in% names(summaries)) {
    model_summary <- summaries |>
      summarise(
        estimate = mean(.data[[metric_col]]),
        .lower = mean(.data$.lower),
        .upper = mean(.data$.upper),
        .by = "model_id"
      ) |>
      mutate(
        model_id = forcats::fct_reorder(
          .data$model_id,
          .data$estimate,
          .desc = FALSE
        )
      )

    plots$by_model <- model_summary |>
      ggplot2::ggplot() +
      ggplot2::aes(.data$model_id, .data$estimate) +
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$estimate),
        show.legend = FALSE,
        size = 3
      ) +
      ggplot2::scale_color_viridis_c() +
      ggplot2::coord_flip() +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::labs(
        x = NULL,
        y = y_label %||% metric_name,
        title = paste(stringr::str_to_title(metric_name), "by model"),
        subtitle = "Points = posterior median, bars = 95% CrI"
      )

    if (!is.null(y_transform)) {
      plots$by_model <- plots$by_model +
        ggplot2::scale_y_continuous(labels = y_transform)
    }
  }

  # Modality comparison plot (averaged over models)
  if ("modality" %in% names(summaries)) {
    modality_summary <- summaries |>
      summarise(
        estimate = mean(.data[[metric_col]]),
        .lower = mean(.data$.lower),
        .upper = mean(.data$.upper),
        .by = "modality"
      )

    plots$by_modality <- modality_summary |>
      ggplot2::ggplot() +
      ggplot2::aes(.data$modality, .data$estimate) +
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$modality),
        size = 4
      ) +
      ggplot2::scale_colour_manual(values = pal, guide = "none") +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::labs(
        x = NULL,
        y = y_label %||% metric_name,
        title = paste("Effect of prompting modality on", metric_name),
        subtitle = "Population-average predictions with 95% CrI"
      )

    if (!is.null(y_transform)) {
      plots$by_modality <- plots$by_modality +
        ggplot2::scale_y_continuous(labels = y_transform)
    }
  }

  # Interaction plot
  if ("model_id" %in% names(summaries) && "modality" %in% names(summaries)) {
    interaction_data <- summaries |>
      mutate(
        model_id = forcats::fct_reorder(
          .data$model_id,
          .data[[metric_col]],
          .desc = FALSE
        )
      )

    plots$interaction <- interaction_data |>
      ggplot2::ggplot() +
      ggplot2::aes(
        .data$model_id,
        .data[[metric_col]],
        colour = .data$modality
      ) +
      ggplot2::geom_linerange(
        ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper),
        position = ggplot2::position_dodge(width = 0.6),
        size = 0.8
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.6),
        size = 3
      ) +
      ggplot2::scale_colour_manual(values = pal, name = "Modality") +
      ggplot2::coord_flip() +
      ggplot2::theme_bw(base_size = 13) +
      ggplot2::labs(
        x = NULL,
        y = y_label %||% metric_name,
        title = paste("Model × modality interaction for", metric_name),
        subtitle = "Population-average predictions with 95% CrI"
      )

    if (!is.null(y_transform)) {
      plots$interaction <- plots$interaction +
        ggplot2::scale_y_continuous(labels = y_transform)
    }
  }

  return(plots)
}

#' Compute correlation between model predictions
#'
#' Calculates posterior correlation between predictions from different models,
#' with optional filtering by parsing quality (e.g., "clean" responses only).
#'
#' @param model1_draws Posterior draws from first model.
#' @param model2_draws Posterior draws from second model.
#' @param filter_parsing If TRUE, filter to clean parsing responses only.
#' @param parsing_data Data with parsing classifications (if filter_parsing = TRUE).
#' @return A tibble with correlation posterior summaries.
#' @export
compute_model_correlation <- function(
  model1_draws,
  model2_draws,
  filter_parsing = FALSE,
  parsing_data = NULL
) {
  # Prepare data for correlation
  if (filter_parsing && !is.null(parsing_data)) {
    # Filter to clean parsing responses only
    clean_responses <- parsing_data |>
      filter(.data$parse_ord == "clean") |>
      distinct(.data$item, .data$model_id, .data$modality)

    model1_draws <- model1_draws |>
      semi_join(clean_responses, by = c("item", "model_id", "modality"))

    model2_draws <- model2_draws |>
      semi_join(clean_responses, by = c("item", "model_id", "modality"))
  }

  # Determine metric columns
  metric1 <- if (".epred" %in% names(model1_draws)) ".epred" else ".prob"
  metric2 <- if (".epred" %in% names(model2_draws)) ".epred" else ".prob"

  # Join draws by common grouping variables
  join_vars <- intersect(
    names(model1_draws),
    c("item", "model_id", "modality", ".draw")
  )

  correlation_data <- model1_draws |>
    select(all_of(c(join_vars, metric1))) |>
    rename(metric1 = all_of(metric1)) |>
    inner_join(
      model2_draws |>
        select(all_of(c(join_vars, metric2))) |>
        rename(metric2 = all_of(metric2)),
      by = join_vars
    )

  # Calculate correlations by draw
  correlation_data |>
    summarise(
      correlation = cor(.data$metric1, .data$metric2, use = "complete.obs"),
      .by = ".draw"
    ) |>
    tidybayes::median_qi(.data$correlation)
}
