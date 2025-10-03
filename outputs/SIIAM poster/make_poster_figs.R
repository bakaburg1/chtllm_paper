# Load required packages to access targets and save figures.
library(targets)
library(here)
library(ggplot2)
library(patchwork)

# Point to the targets data store in the project root.
targets::tar_config_set(store = here::here("_targets"))

# Load in-package helper functions used by stored plot objects (e.g., seq_range).
targets::tar_source(here::here("R"))

# Create the output directory for poster figures.
out_dir <- here::here("outputs", "SIIAM poster", "figs")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# Helper to save ggplot objects to high-resolution PNGs using mm sizes.
save_png <- function(plot, filename, width_mm, height_mm, dpi = 300) {
  # Convert mm to inches for ggsave.
  width_in <- width_mm / 25.4
  height_in <- height_mm / 25.4
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width_in,
    height = height_in,
    dpi = dpi,
    bg = "white"
  )
}

# Load plots from the targets store. These objects are produced by the pipeline.
targets::tar_load(correctness_plots)
targets::tar_load(parsing_plots)
targets::tar_load(consistency_plots)
targets::tar_load(correctness_vs_parsing_plot)
targets::tar_load(correctness_vs_consistency_plot)
targets::tar_load(pareto_frontier_plot)
targets::tar_load(correctness_mosaic_plot)

# Compose combined figures where useful.
# Parsing and consistency side-by-side.
metrics_combo <- (parsing_plots$by_model +
  labs(title = "A. Parsing success")) +
  (consistency_plots$by_model +
    labs(title = "B. Answer consistency")) +
  patchwork::plot_layout(ncol = 2, guides = "collect")

# Accuracy vs Parsing stacked over Accuracy vs Consistency.
cross_metrics <- (correctness_vs_parsing_plot +
  labs(title = "A. Correctness vs parsing")) /
  (correctness_vs_consistency_plot +
    labs(title = "B. Correctness vs consistency"))

# Define sizes aligned to an A0 portrait, 3-column layout.
# One-column width ~ 260 mm, two-column ~ 540 mm, three-column ~ 820 mm.
one_w <- 260
two_w <- 540
three_w <- 820

# Save figures for the poster.
save_png(
  plot = correctness_plots$by_model,
  filename = file.path(out_dir, "overall_correctness.png"),
  width_mm = one_w,
  height_mm = 220
)

save_png(
  plot = correctness_plots$by_modality,
  filename = file.path(out_dir, "modality_correctness.png"),
  width_mm = one_w,
  height_mm = 200
)

save_png(
  plot = metrics_combo,
  filename = file.path(out_dir, "metrics_combo.png"),
  width_mm = two_w,
  height_mm = 220
)

save_png(
  plot = cross_metrics,
  filename = file.path(out_dir, "cross_metrics.png"),
  width_mm = two_w,
  height_mm = 360
)

save_png(
  plot = pareto_frontier_plot,
  filename = file.path(out_dir, "pareto_frontier.png"),
  width_mm = two_w,
  height_mm = 300
)

save_png(
  plot = correctness_mosaic_plot,
  filename = file.path(out_dir, "item_mosaic.png"),
  width_mm = three_w,
  height_mm = 420
)

message("Poster figures saved to: ", out_dir)


