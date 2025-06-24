#' Create comprehensive summary plots
#'
#' Generates publication-ready plots for marginalized posterior summaries,
#' including model comparisons, modality effects, and interaction plots.
#'
#' @param summaries Output from compute_marginalized_summaries().
#' @param group The grouping variable for the plot. Must be one of "model_id",
#' "modality", or "interaction".
#' @param title Title of the plot.
#'
#' @return A ggplot object.
#'
#' @export
plot_summaries <- function(
  summaries,
  group = c("model_id", "modality", "interaction"),
  title = NULL
) {
  # Validate `group` argument
  group <- match.arg(group)

  required_cols <- switch(
    group,
    model_id = "model_id",
    modality = "modality",
    interaction = c("model_id", "modality")
  )

  missing_cols <- setdiff(required_cols, names(summaries))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column{?s} {.var {missing_cols}} not found in `summaries`."
    )
  }

  # Color palette for modalities
  pal <- c(
    cold = "lightblue",
    free = "darkgoldenrod1",
    reasoning = "darkred"
  )


  # Prepare plot data

  plot_data <- summaries


  # Ensure model metadata is available when needed

  missing_meta <- setdiff(c("model_type", "provider"), names(plot_data))
  if (length(missing_meta) > 0 && "model_id" %in% names(plot_data)) {
    models_df <- tryCatch(
      load_models(),
      error = \(e) {
        cli::cli_warn(
          "Unable to retrieve model metadata. Some aesthetics may be missing."
        )
        NULL
      }
    )

    if (!is.null(models_df)) {
      plot_data <- plot_data |>
        dplyr::left_join(
          models_df |>
            dplyr::select("model_id", "model_type", "provider"),
          by = "model_id",
          relationship = "many-to-many"
        )
    }
  }

  # If provider information is available, compute locality (API vs Local)
  if ("provider" %in% names(plot_data)) {
    plot_data <- plot_data |>
      dplyr::mutate(
        locality = ifelse(.data$provider == "ollama", "Local", "API")
      )
  }

  # Reorder models by posterior median score

  plot_data <- plot_data |>
    dplyr::mutate(
      # Re-order factors for nicer display when `model_id` is present.
      dplyr::across(
        .cols = dplyr::any_of("model_id"),
        .fns  = ~ forcats::fct_reorder(.x, .data$.prob, .desc = FALSE)
      )
    )

  # Build the plot

  # Decide x-axis variable
  x_var <- if (group == "modality") "modality" else "model_id"

  # Common base ggplot
  gg <- ggplot2::ggplot(plot_data)

  # Geometry position: dodge only when multiple modalities per model
  dodge <- if (group == "interaction") {
    ggplot2::position_dodge(width = 0.6)
  } else {
    ggplot2::position_identity()
  }

  # Aesthetic mappings
  if ("modality" %in% names(plot_data)) {
    # Colour by modality; optionally shape by model_type if available
    aes_args <- list(
      x = quote(.data$.prob),
      y = quote(.data[[x_var]]),
      colour = quote(.data$modality)
    )
    if ("model_type" %in% names(plot_data)) {
      aes_args$shape <- quote(.data$model_type)
    }
    gg <- gg + ggplot2::aes(!!!aes_args)
  } else if (all(c("model_type", "locality") %in% names(plot_data))) {
    # Colour by model_type, shape by locality (API vs Local)
    gg <- gg +
      ggplot2::aes(
        x = .data$.prob,
        y = .data[[x_var]],
        colour = .data$model_type,
        shape  = .data$locality
      )
  } else {
    # Fallback – basic mapping without extra aesthetics
    gg <- gg + ggplot2::aes(x = .data$.prob, y = .data[[x_var]])
  }

  # Error bars and points
  gg <- gg +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data$.lower, xmax = .data$.upper),
      position = dodge,
      linewidth = 0.8
    ) +
    ggplot2::geom_point(position = dodge, size = 3)

  # Scales
  if ("modality" %in% names(plot_data)) {
    gg <- gg + ggplot2::scale_colour_manual(values = pal, name = "Modality")
    if ("model_type" %in% names(plot_data)) {
      gg <- gg + ggplot2::scale_shape_manual(
        values = c("classic" = 16, "reasoning" = 17),
        name = "Model type"
      )
    }
  } else if (all(c("model_type","locality") %in% names(plot_data))) {
    gg <- gg +
      ggplot2::scale_color_manual(
        values = c(
          "classic" = "goldenrod1",
          "reasoning" = "darkred"
        ),
        name = "Model type"
      ) +
      ggplot2::scale_shape_manual(
        values = c("API" = 16, "Local" = 21),
        name = "Deployment"
      )
  }

  # Format x axis as percentage
  gg <- gg + ggplot2::scale_x_continuous(labels = scales::label_percent(1))

  # Theme, labels, orientation
  gg <- gg +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = "Points = posterior median, bars = 95% CrI"
    )

  return(gg)
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
      color = "darkred",
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
    # All models with aesthetic mappings for frontier, model type, and
    # deployment
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
      color = "darkred",
      size = 3,
      box.padding = 0.5,
      point.padding = 0.3,
      segment.color = "darkred",
      segment.alpha = 0.6
    ) +
    coord_trans(y = "logit") +
    # Manual scales for color, shape, and alpha
    scale_color_manual(
      values = c(
        "Other models" = "darkgoldenrod1", "Pareto frontier" = "darkred")
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

#' Plot correctness by item mosaic
#'
#' Creates a mosaic plot showing model correctness for each item.
#'
#' The plot is a matrix of models versus items, where the fill color represents
#' the median posterior probability of a correct response, and the transparency
#' reflects the certainty of the estimate (1 - CrI width).
#'
#' @param correctness_summaries A data frame containing posterior summaries of
#'   correctness, grouped by model and item. Must include `.prob`, `.lower`, and
#'   `.upper` columns.
#' @param min_alpha The minimum alpha value for transparency (default is 0.25).
#'
#' @return A ggplot object.
#' @export
plot_correctness_mosaic <- function(
  correctness_summaries,
  min_alpha = 0.25
) {
  # Reorder models by average correctness (worst on top, best at bottom)
  model_order <- correctness_summaries |>
    summarise(
      avg_correctness = mean(.data$.prob, na.rm = TRUE),
      .by = "model_id"
    ) |>
    arrange(.data$avg_correctness) |>
    pull(.data$model_id)

  plot_data <- correctness_summaries |>
    mutate(
      # Credible interval size (width)
      cri_width = .data$.upper - .data$.lower,
      # Alpha: higher for smaller CrI (more certainty)
      alpha = 1 - .data$cri_width,
      # Ensure item is a factor sorted numerically
      item = factor(as.numeric(.data$item)),
      # Ensure model_id is a factor with the desired order
      model_id = factor(.data$model_id, levels = model_order)
    )

  ggplot(
    plot_data,
    aes(
      x = .data$item,
      y = .data$model_id,
      fill = .data$.prob
    )
  ) +
    # Use alpha aesthetic in geom_tile
    geom_tile(
      aes(alpha = .data$alpha),
      color = "white",
      linewidth = 0.5
    ) +
    scale_fill_viridis_c(
      name = "Median\nCorrectness",
      labels = scales::percent_format(accuracy = 1)
    ) +
    # Alpha scale ensures transparency is mapped correctly
    scale_alpha_continuous(
      range = c(min_alpha, 1.0), # Map to a visible range
      guide = "none" # Hide alpha legend
    ) +
    labs(
      x = "Benchmark Item ID",
      y = "Model",
      title = "Some questions are inherently harder for all models",
      subtitle = paste(
        "Color indicates median correctness;",
        "transparency reflects certainty (1 - CrI width)."
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}

#' Plot model performance in quadrants
#'
#' Creates a scatter plot that categorizes models into four quadrants based on
#' their performance on two different metrics (e.g., correctness vs.
#' consistency). Models in extreme quartiles are labeled, while those in inner
#' quartiles are more transparent.
#'
#' @param summary_x A data frame with model summaries for the x-axis metric.
#'   Must contain `model_id` and a `.prob` column.
#' @param summary_y A data frame with model summaries for the y-axis metric.
#'   Must contain `model_id` and a `.prob` column.
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param title The plot title.
#' @param x_name Short name for the x-axis metric (for legend labels).
#' @param y_name Short name for the y-axis metric (for legend labels).
#'
#' @return A ggplot object.
#'
#' @export
plot_performance_quadrants <- function(
  summary_x,
  summary_y,
  x_lab,
  y_lab,
  title,
  x_name,
  y_name
) {
  # Join datasets and prepare for plotting
  plot_data <- summary_x |>
    select("model_id", x_metric = ".prob") |>
    left_join(
      summary_y |> select("model_id", y_metric = ".prob"),
      by = "model_id"
    )

  # Calculate medians and quartiles
  median_x <- median(plot_data$x_metric, na.rm = TRUE)
  median_y <- median(plot_data$y_metric, na.rm = TRUE)

  # Create quadrant and labeling variables
  plot_data <- plot_data |>
    mutate(
      quadrant = case_when(
        .data$x_metric > median_x & .data$y_metric > median_y ~ "Top-Right",
        .data$x_metric <= median_x & .data$y_metric > median_y ~ "Top-Left",
        .data$x_metric <= median_x & .data$y_metric <= median_y ~ "Bottom-Left",
        .default = "Bottom-Right"
      ),
      label = .data$model_id
    )

  # Define colors for quadrants
  quadrant_colors <- c(
    "Top-Right" = "darkgreen",
    "Top-Left" = "orange",
    "Bottom-Left" = "darkred",
    "Bottom-Right" = "blue"
  )

  # Create descriptive labels for the legend
  legend_labels <- c(
    "Top-Right" = paste("High", x_name, "& High", y_name),
    "Top-Left" = paste("Low", x_name, "& High", y_name),
    "Bottom-Left" = paste("Low", x_name, "& Low", y_name),
    "Bottom-Right" = paste("High", x_name, "& Low", y_name)
  )

  # Create the plot
  ggplot(
    plot_data,
    aes(
      x = .data$x_metric,
      y = .data$y_metric,
      color = .data$quadrant
    )
  ) +
    geom_vline(xintercept = median_x, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = median_y, linetype = "dashed", alpha = 0.5) +
    geom_point(
      size = 3
    ) +
    ggrepel::geom_text_repel(
      aes(label = .data$label),
      size = 3,
      min.segment.length = 0,
      box.padding = 0.3,
      max.overlaps = 20,
      force = 1.5,
      show.legend = FALSE
    ) +
    scale_x_continuous(
      trans = scales::logit_trans(),
      breaks = \(x) {
        seq_range(plot_data$x_metric, length.out = 8) |> round(3)
      }
    ) +
    scale_y_continuous(
      trans = scales::logit_trans(),
      breaks = \(x) {
        seq_range(plot_data$y_metric, length.out = 8) |> round(3)
      }
    ) +
    scale_color_manual(values = quadrant_colors, labels = legend_labels) +
    scale_alpha_identity() +
    labs(
      x = x_lab,
      y = y_lab,
      title = title,
      subtitle = "Quadrants defined by median performance. Labels for models outside inner quartiles. Axes are on a logit scale.",
      color = "Quadrant"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
}
