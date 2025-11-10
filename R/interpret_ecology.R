#' @importFrom stats aggregate complete.cases cor quantile reorder sd
#' @importFrom utils read.csv write.csv
#' @importFrom graphics plot
#' @import ggplot2
#' @import rayshader
#' @import tidyr
#' @import htmlwidgets
#' @import viridisLite
#' @import readr
#' @importFrom dplyr filter mutate summarise select arrange group_by bind_rows
#' @importFrom tidyr drop_na
#' @importFrom rayshader plot_3d render_snapshot
#' @importFrom viridis scale_fill_viridis
#' @importFrom dplyr mutate select arrange summarise group_by bind_rows
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom rayshader plot_3d render_snapshot
#' @importFrom methods is
#' @importFrom RANN nn2
NULL

#' Interpret model explainability in ecological terms
#'
#' Correlates feature importance values (e.g., SHAP) with canopy metrics
#' derived from voxel data, generating both quantitative and textual insights.
#'
#' @param shap_data A data.frame with at least X_grid, Y_grid, and importance columns.
#' @param eco_metrics Output object from [compute_ecological_metrics()].
#' @param top_n Optional integer (default 10) - number of top drivers to report.
#' @param plot Logical (default TRUE) - whether to visualize correlations.
#' @return A list with correlation table and textual interpretation.
#' @export
#' @examples
#' # Example using synthetic voxel data
#' vox <- data.frame(
#'   X = runif(100, 0, 10),
#'   Y = runif(100, 0, 10),
#'   Z = runif(100, 0, 5),
#'   value = runif(100)
#' )
#' metrics <- compute_ecological_metrics(vox)
#'
#' shap_data <- data.frame(
#'   X_grid = metrics$grid_metrics$X_grid,
#'   Y_grid = metrics$grid_metrics$Y_grid,
#'   importance = runif(nrow(metrics$grid_metrics))
#' )
#'
#' interpret_ecological_drivers(shap_data, metrics)

interpret_ecological_drivers <- function(shap_data, eco_metrics, top_n = 10, plot = TRUE) {
  # --- Checks ---
  stopifnot(is.data.frame(shap_data))
  stopifnot(inherits(eco_metrics, "eco_metrics"))
  if (!all(c("X_grid", "Y_grid", "importance") %in% names(shap_data))) {
    stop("`shap_data` must include columns: X_grid, Y_grid, importance.")
  }

  # --- Align with grid metrics ---
  metrics_df <- eco_metrics$grid_metrics
  merged <- dplyr::left_join(shap_data, metrics_df, by = c("X_grid", "Y_grid"))

  # --- Define ecological metric names ---
  eco_vars <- c("mean_height", "max_height", "sd_height", "canopy_cover", "canopy_complexity")
  eco_vars <- eco_vars[eco_vars %in% colnames(merged)]

  if (length(eco_vars) == 0) stop("No matching ecological metric columns found in eco_metrics$grid_metrics.")

  # --- Compute Spearman correlations ---
  cor_vals <- sapply(eco_vars, function(var) {
    suppressWarnings(cor(merged$importance, merged[[var]], use = "complete.obs", method = "spearman"))
  })

  cor_df <- data.frame(
    metric = eco_vars,
    correlation = as.numeric(cor_vals)
  ) |>
    dplyr::arrange(desc(abs(correlation)))

  # --- Display summary ---
# NA (auto-commented for CRAN build)
  print(cor_df, row.names = FALSE)

  # --- Visualization (optional) ---
  if (plot && requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("viridis", quietly = TRUE)) {
    p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = reorder(metric, correlation), y = correlation, fill = correlation)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_viridis_c(option = "B", direction = 1) +
      ggplot2::labs(
        title = "Ecological Correlation of Model Drivers",
        x = "Ecological Metric",
        y = "Spearman Correlation"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(legend.position = "none")
    print(p)
  }

  # --- Interpret top drivers ---
  top_driver <- cor_df$metric[1]
  strength <- round(abs(cor_df$correlation[1]), 2)
  relation <- ifelse(cor_df$correlation[1] > 0, "positively", "negatively")

  interpret_text <- paste0(
# NA (auto-commented for CRAN build)
    top_driver, " (", relation, " correlated, ? = ", strength, "). ",
    "This suggests that spatial prediction variability is primarily influenced by canopy ",
    ifelse(grepl("height", top_driver), "structure and elevation gradients.", 
           ifelse(grepl("cover", top_driver), "density and surface openness.", 
                  "complexity and heterogeneity."))
  )

  cat("\n", interpret_text, "\n")
  invisible(list(correlations = cor_df, interpretation = interpret_text))
}
