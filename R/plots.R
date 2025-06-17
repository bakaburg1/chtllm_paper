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
plot_summaries <- function(
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
      ggplot() +
      aes(.data$model_id, .data$estimate) +
      geom_linerange(
        aes(ymin = .data$.lower, ymax = .data$.upper),
        linewidth = 0.8
      ) +
      geom_point(
        aes(color = .data$estimate),
        show.legend = FALSE,
        size = 3
      ) +
      scale_color_viridis_c() +
      coord_flip() +
      theme_bw(base_size = 13) +
      labs(
        x = NULL,
        y = y_label %||% metric_name,
        title = paste(stringr::str_to_title(metric_name), "by model"),
        subtitle = "Points = posterior median, bars = 95% CrI"
      )

    if (!is.null(y_transform)) {
      plots$by_model <- plots$by_model +
        scale_y_continuous(labels = y_transform)
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
      ggplot() +
      aes(.data$modality, .data$estimate) +
      geom_linerange(
        aes(ymin = .data$.lower, ymax = .data$.upper),
        linewidth = 0.8
      ) +
      geom_point(
        aes(colour = .data$modality),
        size = 4
      ) +
      scale_colour_manual(values = pal, guide = "none") +
      theme_bw(base_size = 13) +
      labs(
        x = NULL,
        y = y_label %||% metric_name,
        title = paste("Effect of prompting modality on", metric_name),
        subtitle = "Population-average predictions with 95% CrI"
      )

    if (!is.null(y_transform)) {
      plots$by_modality <- plots$by_modality +
        scale_y_continuous(labels = y_transform)
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
      ggplot() +
      aes(
        .data$model_id,
        .data[[metric_col]],
        colour = .data$modality
      ) +
      geom_linerange(
        aes(ymin = .data$.lower, ymax = .data$.upper),
        position = position_dodge(width = 0.6),
        linewidth = 0.8
      ) +
      geom_point(
        position = position_dodge(width = 0.6),
        size = 3
      ) +
      scale_colour_manual(values = pal, name = "Modality") +
      coord_flip() +
      theme_bw(base_size = 13) +
      labs(
        x = NULL,
        y = y_label %||% metric_name,
        title = paste("Model × modality interaction for", metric_name),
        subtitle = "Population-average predictions with 95% CrI"
      )

    if (!is.null(y_transform)) {
      plots$interaction <- plots$interaction +
        scale_y_continuous(labels = y_transform)
    }
  }

  return(plots)
}

#' Create Pareto frontier plot of correctness vs cost
#'
#' Generates a plot showing the trade-off between model correctness and cost,
#' highlighting the Pareto frontier of models that provide optimal performance
#' per unit cost.
#'
#' @param correctness_summaries Correctness summaries by model from
#'   compute_marginalized_summaries().
#' @param models_data Model metadata including cost information.
#'
#' @return A ggplot object showing the Pareto frontier.
#'
#' @export
plot_pareto_frontier <- function(
    correctness_summaries,
    models_data
) {
  rlang::check_installed("ggrepel")

  # Prepare data by joining correctness with cost information
  plot_data <- correctness_summaries |>
    inner_join(
      models_data |>
        select("model_id", "cost_per_mln", "model_type", "provider") |>
        mutate(
          cost_per_mln = tidyr::replace_na(.data$cost_per_mln, 0.001),
          # Hack to avoid re-running the pipeline since gemma 3 is not free
          # anymore
          cost_per_mln = case_when(
            model_id == "gemma-3" ~ 0.2,
            .default = .data$cost_per_mln
          ),
          # Create categorical variables for visualization
          is_local = if_else(.data$provider == "ollama", "(Local)", "(API)"),
          model_type = stringr::str_to_title(.data$model_type)
        ),
      by = "model_id"
    ) |>
    # Use the main probability column (should be .prob)
    rename(correctness = ".prob") |>
    # Compute Pareto frontier status
    # A point is on the Pareto frontier if no other point has both
    # higher correctness AND lower cost
    arrange(.data$cost_per_mln) |>
    mutate(
      max_correctness_so_far = cummax(.data$correctness),
      is_pareto = .data$correctness == .data$max_correctness_so_far,
      frontier_status = if_else(
        .data$is_pareto,
        "Pareto frontier",
        "Other models"
      )
    ) |>
    select(-"max_correctness_so_far")

  # Filter only models at the frontier
  pareto_models <- plot_data |> filter(.data$is_pareto)

  # Create the plot
  plot_data |>
    ggplot(aes(.data$cost_per_mln, .data$correctness)) +
    # Pareto frontier line
    geom_line(
      data = pareto_models,
      aes(linetype = "Empiric"),
      color = "black",
      alpha = .5
    ) +
    annotate(
      "segment",
      x = max(pareto_models$cost_per_mln),
      xend = Inf,
      y = max(pareto_models$correctness),
      yend = max(pareto_models$correctness),
      color = "black",
      alpha = .5
    ) +
    # Connect Pareto frontier with lines
    geom_smooth(
      data = pareto_models,
      aes(linetype = "Fitted"),
      method = "glm",
      method.args = list(family = quasibinomial(link = "logit")),
      color = "red",
      linewidth = 0.8,
      fullrange = TRUE,
      se = FALSE
    ) +
    # Add predictive intervals for all models
    geom_linerange(
      aes(
        ymin = .data$.lower,
        ymax = .data$.upper,
        color = .data$frontier_status
      ),
      linewidth = 0.8,
      alpha = 0.5
    ) +
    # All models with aesthetic mappings for frontier, model type, and deployment
    geom_point(
      aes(
        color = .data$frontier_status,
        shape = interaction(.data$model_type, .data$is_local, sep = " ")
      ),
      size = 3
    ) +
    # Add model labels for Pareto frontier points
    ggrepel::geom_label_repel(
      data = plot_data |> filter(.data$is_pareto),
      aes(label = .data$model_id),
      color = "red",
      size = 3,
      box.padding = 0.5,
      point.padding = 0.3,
      segment.color = "red",
      segment.alpha = 0.6
    ) +
    coord_trans(y = "logit") +
    # Manual scales for color, shape, and alpha
    scale_color_manual(
      values = c("Other models" = "steelblue", "Pareto frontier" = "red")
    ) +
    scale_shape_manual(
      values = c(
        "Classic (API)" = 16,
        "Reasoning (API)" = 17,
        "Classic (Local)" = 21,
        "Reasoning (Local)" = 24
      ),
      name = "Model type"
    ) +
    # Add a legend for line types
    scale_linetype_manual(
      values = c("Empiric" = "solid", "Fitted" = "dashed")
    ) +
    # Reverse legend order and override linetype legend
    guides(
      color = guide_legend(reverse = TRUE),
      shape = guide_legend(reverse = TRUE),
      linetype = guide_legend(
        reverse = TRUE,
        override.aes = list(
          linewidth = 1,
          linetype = 1
        )
      )
    ) +
    # Scaling and labels
    scale_x_log10(
      labels = scales::dollar_format(prefix = "$"),
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      minor_breaks = scales::trans_breaks("log10", function(x) 10^x, n = 10)
    ) +
    scale_y_continuous(
      name = "Correctness rate",
      labels = scales::label_percent(1),
      breaks = \(x) {
        seq_range(plot_data$correctness, length.out = 10) |> round(2)
      }
    ) +
    # Plot titles and theme
    labs(
      x = "Cost per million tokens",
      y = "Correctness rate",
      color = NULL,
      linetype = "Pareto frontier",
      title = "Pareto frontier: Model correctness vs. cost efficiency",
      subtitle = "Optimal models balance high accuracy with low operational costs",
      caption = "Points = individual models; Lines = Pareto-efficient frontiers"
    ) +
    theme_bw(base_size = 13)
}
