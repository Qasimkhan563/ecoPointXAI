# ================================================================
# ecoPointXAI - 3D Explainability Visualization (UTF-safe version)
# ================================================================

#' Visualize voxel-level explainability in 3D (Stable)
#'
#' Creates an interactive 3D visualization of feature attributions
#' from SHAP or gradient-based explanations, with safe text rendering.
#'
#' @param vox_df Data frame with columns X, Y, Z
#' @param explain_result Output from explain_pointnet() (SHAP or Gradients)
#' @param color_by Optional: feature name for coloring
#' @param export_path Optional: .html or .ply file path for export
#' @param point_size Numeric, point size in 3D view
#' @param palette Color palette (default = reversed RdYlBu)
#' @param sample_frac Fraction of points to display (default 0.1)
#' @param z_slice Optional height range (c(minZ, maxZ))
#' @param add_ground Logical, adds green plane beneath
#' @param theme "dark" or "light" background
#'
#' @return Invisible list with voxels, importance, colors
#' @export
visualize_explainability <- function(
  vox_df,
  explain_result,
  color_by = NULL,
  export_path = NULL,
  point_size = 4,
  palette = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
  sample_frac = 0.1,
  z_slice = NULL,
  add_ground = TRUE,
  theme = c("dark", "light")
) {
  # ---------------------------------------------------------------
NA
  # ---------------------------------------------------------------
  theme <- match.arg(theme)
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("Package 'rgl' required.")
  if (!requireNamespace("RColorBrewer", quietly = TRUE))
    stop("Package 'RColorBrewer' required.")

  stopifnot(all(c("X", "Y", "Z") %in% colnames(vox_df)))

  # ---------------------------------------------------------------
  # ?? Determine importance source
  # ---------------------------------------------------------------
  if (!is.null(explain_result$local_explanations)) {
    message("? Using gradient-based importance")
    imp_vals <- rowMeans(abs(explain_result$local_explanations), na.rm = TRUE)

  } else if (!is.null(explain_result$shap_values)) {
    message("? Using SHAP-based importance")
    shap_vals <- explain_result$shap_values
    n_feats <- length(unique(shap_vals$feature))
    if (!"row.id" %in% names(shap_vals)) {
      shap_vals$row.id <- rep(seq_len(nrow(shap_vals) / n_feats), each = n_feats)
    }
    imp_vals <- shap_vals |>
      dplyr::group_by(row.id) |>
      dplyr::summarise(importance = sum(abs(phi), na.rm = TRUE)) |>
      dplyr::pull(importance)
    imp_vals <- imp_vals[seq_len(min(nrow(vox_df), length(imp_vals)))]
  } else {
    stop("Invalid explain_result: must contain SHAP or gradient outputs.")
  }

  vox_df$importance <- imp_vals

  # ---------------------------------------------------------------
NA
  # ---------------------------------------------------------------
  vox_df <- vox_df |> dplyr::filter(!is.na(importance))
  if (!is.null(z_slice)) {
    vox_df <- vox_df |> dplyr::filter(Z >= z_slice[1], Z <= z_slice[2])
  }
  if (sample_frac < 1) {
    vox_df <- vox_df[sample(seq_len(nrow(vox_df)),
                            size = floor(sample_frac * nrow(vox_df))), ]
  }

  # remove numeric outliers (crazy towers)
  z_q <- quantile(vox_df$Z, probs = c(0.001, 0.999), na.rm = TRUE)
  vox_df <- vox_df |> dplyr::filter(Z >= z_q[1], Z <= z_q[2])

  # ---------------------------------------------------------------
NA
  # ---------------------------------------------------------------
  imp_norm <- (vox_df$importance - min(vox_df$importance, na.rm = TRUE)) /
              (max(vox_df$importance, na.rm = TRUE) - min(vox_df$importance, na.rm = TRUE) + 1e-8)
  cols <- palette[cut(imp_norm, breaks = length(palette), labels = FALSE)]

  # ---------------------------------------------------------------
NA
  # ---------------------------------------------------------------
  rgl::open3d()
  bg_col <- if (theme == "dark") "black" else "white"
  txt_col <- if (theme == "dark") "white" else "black"
  rgl::bg3d(color = bg_col)

  if (add_ground) {
    gx <- seq(min(vox_df$X), max(vox_df$X), length.out = 20)
    gy <- seq(min(vox_df$Y), max(vox_df$Y), length.out = 20)
    gz <- matrix(min(vox_df$Z), nrow = 20, ncol = 20)
    rgl::surface3d(gx, gy, gz, color = "#1e5631", alpha = 0.9)
  }

  rgl::points3d(vox_df$X, vox_df$Y, vox_df$Z,
                col = cols, size = point_size)
  rgl::axes3d(col = txt_col)
  rgl::title3d(
    main = "ecoPointXAI Explainability Field",
    xlab = "X (m)", ylab = "Y (m)", zlab = "Height (m)",
    color = txt_col
  )

  # ---------------------------------------------------------------
NA
  # ---------------------------------------------------------------
  if (!is.null(export_path)) {
    dir.create(dirname(export_path), recursive = TRUE, showWarnings = FALSE)
    if (grepl("\\.html$", export_path, ignore.case = TRUE)) {
      htmlwidgets::saveWidget(rgl::rglwidget(), export_path, selfcontained = TRUE)
      message("? HTML export ? ", export_path)
    } else if (grepl("\\.ply$", export_path, ignore.case = TRUE)) {
      rgl::writePLY(rgl::scene3d(), export_path)
      message("? PLY export ? ", export_path)
    } else {
      warning("Unsupported export format (use .html or .ply)")
    }
  }

  message("? Visualization complete - rotate and zoom interactively.")
  invisible(list(voxels = vox_df, importance = vox_df$importance, colors = cols))
}
