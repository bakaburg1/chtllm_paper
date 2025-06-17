#' Create a publication-ready summary table using gt
#'
#' Formats marginalized posterior summaries into a polished gt table, ready for
#' publication or reporting.
#'
#' @param summaries A data frame containing summarized data. It should have
#'   columns for model_id, modality (optional), a metric, and credible intervals
#'   (.lower, .upper).
#' @param metric_name A character string for the primary metric column name.
#' @param title The main title for the table.
#' @param subtitle The subtitle for the table.
#' @param source_note A source note or caption for the table.
#' @param group_by_var A variable to group and spanner-label the table by. Can
#'   be "modality" or NULL.
#'
#' @return A gt_tbl object.
#'
#' @examples
#' \dontrun{
#' # Create a correctness table grouped by modality
#' correctness_table <- create_summary_table(
#'   summaries = summaries_correctness_interaction,
#'   metric_name = ".prob",
#'   title = "Model Correctness",
#'   subtitle = "Probability of generating the correct answer",
#'   source_note = "Summaries are posterior medians with 95% CrIs.",
#'   group_by_var = "modality"
#' )
#'
#' # Create a parsing quality table
#' parsing_table <- create_summary_table(
#'   summaries = summaries_parsing_interaction,
#'   metric_name = ".prob",
#'   title = "Clean Parsing Rate",
#'   subtitle = "Probability of responses being cleanly parsed",
#'   source_note = "Based on multinomial parsing model results.",
#'   group_by_var = "modality"
#' )
#'
#' # Create a simple table without grouping
#' model_comparison <- create_summary_table(
#'   summaries = summaries_correctness_by_model,
#'   metric_name = ".prob",
#'   title = "Model Performance Comparison",
#'   subtitle = "Overall correctness across all modalities",
#'   source_note = "Marginalized over prompting modalities."
#' )
#' }
#'
#' @export
create_summary_table <- function(
    summaries,
    metric_name,
    title,
    subtitle,
    source_note,
    group_by_var = NULL
) {
  rlang::check_installed("gt")

  # Create percentage formatter
  pct_fmt <- scales::label_percent(accuracy = 0.1)

  # Define a color palette for modalities if present
  pal <- c(
    cold = "lightblue",
    free = "darkgoldenrod1",
    reasoning = "darkred"
  )

  # Check for metric and interval columns
  required_cols <- c("model_id", metric_name, ".lower", ".upper")
  if (!all(required_cols %in% names(summaries))) {
    cli::cli_abort(
      "The `summaries` data frame must contain: {required_cols}"
    )
  }

  # Prepare data: format credible intervals, rename metric, and order by metric
  table_data <- summaries |>
    mutate(
      # Format credible intervals as percentages
      ci = paste0(
        pct_fmt(.data$.lower),
        " - ",
        pct_fmt(.data$.upper)
      ),
      # Format main metric as percentage
      across(
        all_of(metric_name),
        ~ pct_fmt(.x)
      )
    ) |>
    rename(metric = all_of(metric_name)) |>
    # Order by metric in descending order (highest values first)
    arrange(desc(.data$metric)) |>
    select(
      any_of(c("model_id", "modality")),
      "metric",
      "ci"
    )

  # Start building the gt table
  gt_table <- table_data |>
    gt::gt(
      groupname_col = if (is.null(group_by_var)) character(0) else group_by_var
    ) |>
    # Add title and subtitle
    gt::tab_header(
      title = gt::md(paste0("**", title, "**")),
      subtitle = subtitle
    ) |>
    # Add source note
    gt::tab_source_note(
      source_note = gt::md(source_note)
    ) |>
    # Rename columns for presentation
    gt::cols_label(
      model_id = "Model",
      metric = "Estimate",
      ci = "95% CrI"
    ) |>
    # Center-align metric columns
    gt::cols_align(
      align = "center",
      columns = c("metric", "ci")
    ) |>
    # Format the table with common styling
    gt::tab_options(
      table.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = gt::px(2),
      table_body.border.bottom.color = "black",
      heading.border.bottom.color = "black",
      table.width = gt::pct(100)
    )

  # Apply modality-specific coloring if modality is a grouping variable
  if (!is.null(group_by_var) && "modality" %in% names(summaries)) {
    for (mod in names(pal)) {
      if (mod %in% unique(summaries[[group_by_var]])) {
        gt_table <- gt_table |>
          gt::tab_style(
            style = gt::cell_fill(color = pal[[mod]], alpha = 0.2),
            locations = gt::cells_row_groups(groups = mod)
          )
      }
    }
  }

  gt_table
}

#' Save gt table to a file
#'
#' Saves a gt table to a specified file path, creating the directory if it does
#' not exist.
#'
#' @param gt_table The gt table object to save.
#' @param path The file path for saving the table (e.g.,
#'   "outputs/tables/my_table.html").
#'
#' @return The file path of the saved table.
#'
#' @examples
#' \dontrun{
#' # Create a table and save it as HTML
#' my_table <- create_summary_table(
#'   summaries = summaries_correctness_interaction,
#'   metric_name = ".prob",
#'   title = "Model Correctness",
#'   subtitle = "Probability of generating the correct answer",
#'   source_note = "Summaries are posterior medians with 95% CrIs.",
#'   group_by_var = "modality"
#' )
#'
#' # Save to HTML file
#' save_gt_table(my_table, "outputs/tables/correctness.html")
#'
#' # Save to different formats
#' save_gt_table(my_table, "outputs/tables/correctness.pdf")
#' save_gt_table(my_table, "outputs/tables/correctness.png")
#' save_gt_table(my_table, "outputs/tables/correctness.rtf")
#'
#' # Use in targets pipeline
#' tar_target(
#'   table_file,
#'   save_gt_table(
#'     my_table,
#'     path = here::here("outputs", "tables", "my_table.html")
#'   ),
#'   format = "file"
#' )
#' }
#'
#' @export
save_gt_table <- function(gt_table, path) {
  # Create the directory if it doesn't exist
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE)
  }

  # Save the table
  gt::gtsave(gt_table, path)

  # Return the path for targets
  path
}
