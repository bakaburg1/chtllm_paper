#' Create a flextable summary table for posterior estimates
#'
#' Creates a formatted flextable displaying posterior estimates with confidence
#' intervals. The table includes percentage formatting and customizable headers.
#'
#' @param summaries A data frame containing posterior summaries with columns for
#'   the metric, lower bound (.lower), and upper bound (.upper)
#' @param metric_name Character string specifying the name of the metric column
#'   in `summaries`
#' @param caption Optional character string for table caption
#' @param merge_vars Optional character vector of column names to merge
#'   vertically in the table (useful for grouping rows)
#' @param label_overrides Named list of custom column labels to override
#'   defaults
#'
#' @return A flextable object with formatted posterior estimates
#'
#' @details The function expects the input data frame to contain at minimum:
#' - A column with the metric values (specified by `metric_name`)
#' - `.lower` and `.upper` columns for confidence interval bounds
#'
#' Default column labels can be overridden using the `label_overrides`
#' parameter. The function automatically formats values as percentages and
#' creates a confidence interval column.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' create_summary_table(
#'   summaries = posterior_data,
#'   metric_name = "accuracy"
#' )
#'
#' # With custom labels and merging
#' create_summary_table(
#'   summaries = posterior_data,
#'   metric_name = "accuracy",
#'   caption = "Model Performance Summary",
#'   merge_vars = c("model_type"),
#'   label_overrides = list(model_id = "Model Name")
#' )
#' }
#'
#' @export
create_summary_table <- function(
  summaries,
  metric_name,
  caption = NULL,
  merge_vars = NULL,
  label_overrides = list()
) {
  # Validate required columns are present
  required_cols <- c(metric_name, ".lower", ".upper")
  missing_cols <- setdiff(required_cols, names(summaries))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Missing required columns in `summaries` for flextable creation.",
        "i" = "Expected columns: {required_cols}",
        "x" = "Missing columns: {missing_cols}"
      )
    )
  }

  # Set up percentage formatting function
  pct_fmt <- scales::label_percent(accuracy = 0.1)

  # Transform data: create estimate and CI columns, remove raw metric columns
  table_data <- summaries |>
    dplyr::mutate(
      estimate = pct_fmt(.data[[metric_name]]),
      ci = paste0(pct_fmt(.data$.lower), " - ", pct_fmt(.data$.upper))
    ) |>
    dplyr::relocate("estimate", "ci", .after = dplyr::last_col()) |>
    dplyr::select(
      -dplyr::all_of(metric_name),
      -dplyr::any_of(c(".lower", ".upper", ".width"))
    ) |>
    dplyr::select(-dplyr::any_of(c(".point", ".interval"))) |>
    as.data.frame()

  # Set up column labels with defaults and user overrides
  label_defaults <- list(
    model_id = "Model ID",
    model_type = "Model type",
    modality = "Prompt strategy",
    estimate = "Estimate",
    ci = "95% CrI"
  )
  header_labels <- utils::modifyList(
    label_defaults,
    label_overrides,
    keep.null = TRUE
  )
  # Only keep labels for columns that exist in the data
  header_labels <- header_labels[names(header_labels) %in% names(table_data)]
  header_labels <- as.list(header_labels)

  # Create base flextable
  ft <- flextable::flextable(table_data)

  # Apply custom header labels if any exist
  if (length(header_labels) > 0) {
    ft <- flextable::set_header_labels(ft, values = header_labels)
  }

  # Set column alignments: numeric columns centered, text columns left-aligned
  numeric_cols <- intersect(c("estimate", "ci"), names(table_data))
  if (length(numeric_cols) > 0) {
    ft <- flextable::align(ft, j = numeric_cols, align = "center")
  }

  text_cols <- setdiff(names(table_data), numeric_cols)
  if (length(text_cols) > 0) {
    ft <- flextable::align(ft, j = text_cols, align = "left")
  }

  # Apply vertical merging for specified variables (useful for grouping)
  if (!is.null(merge_vars)) {
    merge_vars <- intersect(merge_vars, names(table_data))
    if (length(merge_vars) > 0) {
      ft <- ft |>
        flextable::merge_v(j = merge_vars) |>
        flextable::valign(j = merge_vars, valign = "top")
    }
  }

  # Apply consistent styling
  ft <- ft |>
    flextable::theme_vanilla() |>
    flextable::fontsize(part = "header", size = 11) |>
    flextable::fontsize(part = "body", size = 10) |>
    flextable::padding(padding = 4) |>
    flextable::autofit(add_w = 0.2, add_h = 0.05)

  # Add caption if provided
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption)
  }

  ft
}

#' Create a combined performance table across metrics
#'
#' Builds a flextable with median posterior accuracy, parsing success, and
#' consistency side by side for each model. Includes color-coded background
#' gradients to highlight performance differences.
#'
#' @param correctness Data frame with model accuracy posteriors containing
#'   columns: model_id, .prob, .lower, .upper
#' @param parsing Data frame with parsing success posteriors with same structure
#' @param consistency Data frame with consistency posteriors with same structure
#' @param caption Optional character string for table caption
#'
#' @return A flextable object with combined performance metrics and color coding
#'
#' @details The function creates a comprehensive performance table by: 1.
#'   Joining the three metric datasets by model_id 2. Formatting values as
#'   percentages with confidence intervals 3. Applying divergent color gradients
#'   based on median values 4. Sorting models by accuracy (descending)
#'
#' Color coding uses a light divergent palette:
#' - Lower values: light orange to white
#' - Higher values: white to light blue
#' - Gradient switches at the median value for each metric
#'
#' @examples
#' \dontrun{
#' create_model_performance_table(
#'   correctness = accuracy_posteriors,
#'   parsing = parsing_posteriors,
#'   consistency = consistency_posteriors,
#'   caption = "Model Performance Comparison"
#' )
#' }
#'
#' @export
create_model_performance_table <- function(
  correctness,
  parsing,
  consistency,
  caption = NULL
) {
  # Set up percentage formatting
  pct_fmt <- scales::label_percent(accuracy = 0.1)

  # Helper function to format cells with median [lower - upper] format
  fmt_cell <- function(median, lower, upper) {
    paste0(
      pct_fmt(median),
      " [",
      pct_fmt(lower),
      " \u2013 ", # en dash
      pct_fmt(upper),
      "]"
    )
  }

  # Helper function to extract and rename metric columns
  pull_metric <- function(df, prefix) {
    df |>
      dplyr::transmute(
        model_id = .data$model_id,
        !!paste0(prefix, "_median") := .data$.prob,
        !!paste0(prefix, "_lower") := .data$.lower,
        !!paste0(prefix, "_upper") := .data$.upper
      )
  }

  # Join all metrics by model_id and sort by accuracy
  joined <- pull_metric(correctness, "accuracy") |>
    dplyr::inner_join(pull_metric(parsing, "parsing"), by = "model_id") |>
    dplyr::inner_join(
      pull_metric(consistency, "consistency"),
      by = "model_id"
    ) |>
    dplyr::arrange(dplyr::desc(.data$accuracy_median))

  # Create formatted display columns while keeping raw values for coloring
  table_data <- joined |>
    dplyr::mutate(
      Accuracy = fmt_cell(
        .data$accuracy_median,
        .data$accuracy_lower,
        .data$accuracy_upper
      ),
      Parsing = fmt_cell(
        .data$parsing_median,
        .data$parsing_lower,
        .data$parsing_upper
      ),
      Consistency = fmt_cell(
        .data$consistency_median,
        .data$consistency_lower,
        .data$consistency_upper
      )
    ) |>
    dplyr::select(
      "Model ID" = "model_id",
      "accuracy_median",
      "Accuracy",
      "parsing_median",
      "Parsing",
      "consistency_median",
      "Consistency"
    )

  # Function to create divergent color gradient based on median split
  get_gradient <- function(values) {
    # Two light divergent colors with a switch at the median
    # Low values: light orange → white; High values: white → light blue
    low_col <- "#fde2cf" # light orange
    high_col <- "#d6eaf8" # light blue

    rng <- range(values, na.rm = TRUE)
    vmin <- rng[1]
    vmax <- rng[2]
    vmid <- stats::median(values, na.rm = TRUE)

    # Handle degenerate ranges gracefully (all values equal)
    if (!is.finite(vmin) || !is.finite(vmax) || isTRUE(all.equal(vmin, vmax))) {
      return(rep(high_col, length(values)))
    }

    # Create color palettes for each side of the median
    pal_low <- grDevices::colorRampPalette(c(low_col, "#ffffff"))
    pal_high <- grDevices::colorRampPalette(c("#ffffff", high_col))

    n <- length(values)
    cols <- character(n)
    # Avoid division by zero when all values fall on one side of median
    denom_low <- max(vmid - vmin, .Machine$double.eps)
    denom_high <- max(vmax - vmid, .Machine$double.eps)

    # Assign colors based on position relative to median
    for (i in seq_len(n)) {
      x <- values[i]
      if (!is.finite(x)) {
        cols[i] <- "#ffffff" # White for missing values
      } else if (x <= vmid) {
        # Scale from minimum to median
        t <- (x - vmin) / denom_low
        cols[i] <- pal_low(256)[max(1L, min(256L, floor(t * 255) + 1L))]
      } else {
        # Scale from median to maximum
        t <- (x - vmid) / denom_high
        cols[i] <- pal_high(256)[max(1L, min(256L, floor(t * 255) + 1L))]
      }
    }
    cols
  }

  # Generate color gradients for each metric
  acc_cols <- get_gradient(table_data$accuracy_median)
  pars_cols <- get_gradient(table_data$parsing_median)
  cons_cols <- get_gradient(table_data$consistency_median)

  # Remove median columns from display (keep only formatted versions)
  display_data <- table_data |>
    dplyr::select(-dplyr::ends_with("_median"))

  # Create flextable and apply styling
  ft <- flextable::flextable(display_data)

  ft <- ft |>
    # Apply background colors for each metric column
    flextable::bg(
      i = seq_len(nrow(display_data)),
      j = "Accuracy",
      bg = acc_cols
    ) |>
    flextable::bg(
      i = seq_len(nrow(display_data)),
      j = "Parsing",
      bg = pars_cols
    ) |>
    flextable::bg(
      i = seq_len(nrow(display_data)),
      j = "Consistency",
      bg = cons_cols
    ) |>
    # Set header labels
    flextable::set_header_labels(
      Model = "Model ID",
      Accuracy = "Accuracy",
      Parsing = "Parsing success",
      Consistency = "Consistency"
    ) |>
    # Set column alignments
    flextable::align(
      j = c("Accuracy", "Parsing", "Consistency"),
      align = "center"
    ) |>
    flextable::align(j = "Model ID", align = "left") |>
    # Apply consistent theme and formatting
    flextable::theme_vanilla() |>
    flextable::fontsize(part = "header", size = 11) |>
    flextable::fontsize(part = "body", size = 10) |>
    flextable::padding(padding = 4) |>
    flextable::autofit(add_w = 0.2, add_h = 0.05)

  # Add caption if provided
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption)
  }

  ft
}

#' Create flextable for benchmark questions
#'
#' Creates a formatted table displaying benchmark questions with their options,
#' highlighting correct answers and organizing by item and source.
#'
#' @param questions Data frame containing benchmark questions with columns:
#'   item, source, item_text, option_A, option_B, option_C, option_D,
#'   option_correct
#'
#' @return A flextable object with formatted questions and highlighted correct
#'   answers
#'
#' @details The function transforms the questions data from wide to long format,
#'   creating one row per option. Features include:
#' - Vertical merging of item, source, and question text for clean layout
#' - Green highlighting and bold formatting for correct answers
#' - Horizontal lines separating question groups
#' - Fixed column widths optimized for readability
#'
#' @examples
#' \dontrun{
#' create_questions_table(benchmark_questions)
#' }
#'
#' @export
create_questions_table <- function(questions) {
  # Transform from wide to long format: one row per option
  question_long <- questions |>
    dplyr::mutate(Item = as.integer(.data$item)) |>
    tidyr::pivot_longer(
      cols = c("option_A", "option_B", "option_C", "option_D"),
      names_to = "Option",
      values_to = "Choice"
    ) |>
    dplyr::mutate(
      # Clean up option labels (A, B, C, D)
      Option = stringr::str_to_upper(stringr::str_remove(
        .data$Option,
        "option_"
      )),
      # Flag correct answers for highlighting
      is_correct = .data$Option == .data$option_correct
    ) |>
    dplyr::arrange(.data$Item, .data$Option) |>
    dplyr::select(
      "Item",
      Source = "source",
      Question = "item_text",
      "Option",
      "Choice",
      "is_correct"
    ) |>
    # Create composite key for source display
    dplyr::mutate(source_key = paste(.data$Item, .data$Source, sep = "::"))

  # Create flextable with selected columns
  ft <- flextable::flextable(
    question_long,
    col_keys = c("Item", "source_key", "Question", "Option", "Choice")
  )

  ft <- ft |>
    # Set column headers
    flextable::set_header_labels(
      Item = "Item",
      source_key = "Source",
      Question = "Question",
      Option = "Option",
      Choice = "Answer option"
    ) |>
    # Merge cells vertically for item, question, and source (one per question)
    flextable::merge_v(j = c("Item", "Question", "source_key")) |>
    flextable::valign(
      j = c("Item", "Question", "source_key"),
      valign = "top"
    ) |>
    # Custom composition for source column to display just the source name
    flextable::compose(
      j = "source_key",
      value = flextable::as_paragraph(flextable::as_chunk(question_long$Source))
    ) |>
    # Set column alignments
    flextable::align(j = c("Item", "source_key", "Option"), align = "center") |>
    # Apply consistent theme
    flextable::theme_vanilla() |>
    flextable::fontsize(part = "header", size = 11) |>
    flextable::fontsize(part = "body", size = 10) |>
    flextable::height(height = 0.35, part = "body") |>
    flextable::autofit(add_w = 0.2, add_h = 0.05) |>
    # Add horizontal lines after each question (after option D)
    flextable::hline(
      i = which(question_long$Option == "D"),
      border = officer::fp_border(color = "black", width = 2)
    ) |>
    # Set fixed column widths for optimal layout
    flextable::width(j = "Question", width = 5.2) |>
    flextable::width(j = "Choice", width = 3.5) |>
    flextable::width(j = "Item", width = 0.5) |>
    flextable::width(j = c("source_key", "Option"), width = 1) |>
    flextable::padding(padding = 4)

  # Highlight correct options with green background and bold text
  idx <- which(question_long$is_correct)
  if (length(idx)) {
    ft <- ft |>
      flextable::bg(i = idx, j = c("Option", "Choice"), bg = "#e8f4ea") |>
      flextable::bold(i = idx, j = c("Option", "Choice"), bold = TRUE)
  }

  ft
}

#' Create flextable for model configurations
#'
#' Creates a formatted table displaying model configuration details including
#' provider, endpoint, type, cost, and search capabilities.
#'
#' @param models Data frame containing model configurations with columns:
#'   model_id, provider, model, model_type, cost_per_mln, search_capability,
#'   active
#'
#' @return A flextable object with formatted model configuration information
#'
#' @details The function processes model configuration data by:
#' - Filtering out parser models
#' - Formatting cost information with special handling for free/promotional
#' models
#' - Converting text fields to proper case
#' - Sorting by provider, cost, type, and model name
#'
#' Cost formatting includes special cases:
#' - "0$ (promo)" for promotional free models
#' - "0$ (local)" for local Ollama models
#' - "N/A" for missing cost information
#'
#' @examples
#' \dontrun{
#' create_models_table(model_configs)
#' }
#'
#' @export
create_models_table <- function(models) {
  models |>
    # Remove parser model from display
    dplyr::filter(.data$model_id != "parser-model") |>
    dplyr::select(-"active") |>
    # Sort by provider, cost (descending), type, and model name
    dplyr::arrange(
      dplyr::desc(.data$provider),
      dplyr::desc(.data$cost_per_mln),
      .data$model_type,
      .data$model
    ) |>
    dplyr::mutate(
      # Convert model type to title case
      Type = stringr::str_to_title(.data$model_type),
      # Format cost with special handling for free models
      cost_per_mln = dplyr::case_when(
        is.na(.data$cost_per_mln) ~ "N/A",
        .data$cost_per_mln == 0 & .data$provider != "ollama" ~ "0$ (promo)",
        .data$cost_per_mln == 0 & .data$provider == "ollama" ~ "0$ (local)",
        TRUE ~ scales::dollar(as.numeric(.data$cost_per_mln))
      ),
      # Convert search capability to sentence case
      search_capability = stringr::str_to_sentence(.data$search_capability)
    ) |>
    # Select and rename columns for display
    dplyr::select(
      "Model ID" = "model_id",
      "Provider" = "provider",
      "Endpoint / deployment" = "model",
      "Type",
      "Cost\n(USD per mln tokens)" = "cost_per_mln",
      "Search\ncapability" = "search_capability"
    ) |>
    # Create and style flextable
    flextable::flextable() |>
    flextable::align(
      j = c("Provider", "Type", "Search\ncapability"),
      align = "center"
    ) |>
    flextable::theme_vanilla() |>
    flextable::fontsize(part = "header", size = 11) |>
    flextable::fontsize(part = "body", size = 10) |>
    flextable::autofit(add_w = 0.2, add_h = 0.05) |>
    flextable::padding(padding = 4)
}

#' Create flextable for model × prompt interaction performance
#'
#' Creates a detailed performance table showing how different models perform
#' across different prompting strategies (cold, free, reasoning) for multiple
#' metrics.
#'
#' @param correctness_interaction Data frame with model-prompt accuracy
#'   interactions containing columns: model_id, modality, .prob, .lower, .upper
#' @param parsing_interaction Data frame with parsing success interactions (same
#'   structure)
#' @param consistency_interaction Data frame with consistency interactions (same
#'   structure)
#' @param caption Optional character string for table caption
#'
#' @return A flextable object showing performance across model-prompt
#'   combinations
#'
#' @details This function creates a comprehensive interaction table by: 1.
#'   Joining all three metrics by model_id and modality (prompt strategy) 2.
#'   Formatting values as percentages with confidence intervals 3. Ordering
#'   modalities as: cold, free, reasoning 4. Sorting by overall accuracy
#'   performance
#'
#' The table helps identify which prompting strategies work best for different
#' models across multiple performance dimensions.
#'
#' @examples
#' \dontrun{
#' create_interaction_performance_table(
#'   correctness_interaction = accuracy_by_prompt,
#'   parsing_interaction = parsing_by_prompt,
#'   consistency_interaction = consistency_by_prompt,
#'   caption = "Model Performance by Prompting Strategy"
#' )
#' }
#'
#' @export
create_interaction_performance_table <- function(
  correctness_interaction,
  parsing_interaction,
  consistency_interaction,
  caption = NULL
) {
  # Set up formatting functions
  pct_fmt <- scales::label_percent(accuracy = 0.1)
  fmt_cell <- function(m, l, u) {
    paste0(pct_fmt(m), " [", pct_fmt(l), " \u2013 ", pct_fmt(u), "]")
  }

  # Helper function to normalize metric data frames
  norm <- function(df, prefix) {
    df |>
      dplyr::transmute(
        model_id = .data$model_id,
        modality = .data$modality,
        !!paste0(prefix, "_median") := .data$.prob,
        !!paste0(prefix, "_lower") := .data$.lower,
        !!paste0(prefix, "_upper") := .data$.upper
      )
  }

  # Join all metrics by model_id and modality (prompt strategy)
  joined <- norm(correctness_interaction, "acc") |>
    dplyr::inner_join(
      norm(parsing_interaction, "pars"),
      by = c("model_id", "modality")
    ) |>
    dplyr::inner_join(
      norm(consistency_interaction, "cons"),
      by = c("model_id", "modality")
    ) |>
    dplyr::mutate(
      # Set factor levels for consistent ordering of prompt strategies
      modality = factor(.data$modality, levels = c("cold", "free", "reasoning"))
    ) |>
    # Sort by accuracy performance (best first)
    dplyr::arrange(dplyr::desc(.data$acc_median))

  # Create formatted display columns
  table_data <- joined |>
    dplyr::mutate(
      Accuracy = fmt_cell(.data$acc_median, .data$acc_lower, .data$acc_upper),
      `Parsing success` = fmt_cell(
        .data$pars_median,
        .data$pars_lower,
        .data$pars_upper
      ),
      Consistency = fmt_cell(
        .data$cons_median,
        .data$cons_lower,
        .data$cons_upper
      )
    ) |>
    dplyr::select(
      "Model ID" = "model_id",
      Strategy = "modality",
      "Accuracy",
      "Parsing success",
      "Consistency"
    )

  # Create and style flextable
  ft <- flextable::flextable(table_data) |>
    flextable::align(
      j = c("Accuracy", "Parsing success", "Consistency", "Strategy"),
      align = "center"
    ) |>
    flextable::align(j = "Model ID", align = "left") |>
    flextable::theme_vanilla() |>
    flextable::fontsize(part = "header", size = 11) |>
    flextable::fontsize(part = "body", size = 10) |>
    flextable::padding(padding = 4) |>
    flextable::autofit(add_w = 0.2, add_h = 0.05)

  # Add caption if provided
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption)
  }
  ft
}
