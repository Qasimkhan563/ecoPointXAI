visualize_voxel_importance_3D <- function(
  vox_df,
  explain_result,
  method = c("gradients", "shap"),
  cube_size = 1,
  alpha_range = c(0.3, 1),
  palette = "viridis",
  export_path = NULL
) {
  stopifnot(requireNamespace("rgl"), requireNamespace("viridisLite"))
  method <- match.arg(method)

  # --- Compute importance
  if (method == "gradients") {
    imp_vals <- rowMeans(abs(explain_result$local_explanations), na.rm = TRUE)
  } else {
    shap_vals <- explain_result$shap_values
    n_feats <- length(unique(shap_vals$feature))
    if (!"row.id" %in% names(shap_vals))
      shap_vals$row.id <- rep(seq_len(nrow(shap_vals) / n_feats), each = n_feats)
    imp_vals <- shap_vals |>
      dplyr::group_by(row.id) |>
      dplyr::summarise(importance = sum(abs(phi), na.rm = TRUE)) |>
      dplyr::pull(importance)
  }

  imp_vals <- imp_vals[seq_len(nrow(vox_df))]
  imp_vals[!is.finite(imp_vals)] <- 0
  vox_df$importance <- imp_vals

  # --- Remove duplicates, NAs, zero height
  vox_df <- vox_df |>
    dplyr::group_by(X, Y) |>
    dplyr::summarise(Z = mean(Z, na.rm = TRUE),
                     importance = mean(importance, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(is.finite(X), is.finite(Y), is.finite(Z)) |>
    dplyr::filter(Z > 0)

  # --- Normalize
  imp_norm <- (vox_df$importance - min(vox_df$importance)) /
              (max(vox_df$importance) - min(vox_df$importance) + 1e-8)
  color_vals <- viridisLite::viridis(100)[cut(imp_norm, 100, labels = FALSE)]
  alpha_vals <- alpha_range[1] + imp_norm * diff(alpha_range)

  # --- Scene
  rgl::open3d()
  rgl::bg3d("#d9f0ff")
  rgl::material3d(fog = TRUE, fogtype = "exp2", fogdensity = 0.02, fogcolor = "#d9f0ff")
  rgl::light3d(theta = 35, phi = 45, diffuse = "#fff8dc")
  rgl::axes3d(col = "gray40")
  rgl::title3d("Virtual Forest", "X (m)", "Y (m)", "Height (m)")

  # --- Terrain
  gx <- seq(min(vox_df$X), max(vox_df$X), length.out = 40)
  gy <- seq(min(vox_df$Y), max(vox_df$Y), length.out = 40)
  gz <- outer(gx, gy, function(x, y) 0.3 * sin(x / 10) * cos(y / 12))
  rgl::surface3d(gx, gy, gz, color = "#2f5d32")

  # --- Cone helper (avoids missing vertices)
  cone3d_safe <- function(base, height, radius, sides = 14) {
    theta <- seq(0, 2 * pi, length.out = sides)
    x <- base[1] + radius * cos(theta)
    y <- base[2] + radius * sin(theta)
    z <- rep(base[3], sides)
    tip <- c(base[1], base[2], base[3] + height)
    verts <- rbind(cbind(x, y, z), tip)
    faces <- cbind(1:(sides - 1), 2:sides, rep(sides + 1, sides - 1))
    faces <- rbind(faces, c(sides, 1, sides + 1))
    rgl::tmesh3d(vertices = t(verts), indices = t(faces), homogeneous = FALSE)
  }

  # --- Render forest safely
NA
  n_before <- nrow(vox_df)
  for (i in seq_len(n_before)) {
    cx <- vox_df$X[i]; cy <- vox_df$Y[i]; cz <- 0
    h <- cube_size * (0.8 + 3 * imp_norm[i])
    r <- cube_size * (0.07 + 0.15 * imp_norm[i])

    if (is.na(cx) || is.na(cy) || is.na(h) || h <= 0) next

    # skip near-zero segments
    top_z <- cz + h * 0.7
    if (abs(top_z - cz) < 1e-6) next

    col_i <- grDevices::adjustcolor(color_vals[i], alpha.f = alpha_vals[i])
    trunk <- rgl::cylinder3d(matrix(c(cx, cy, cz, cx, cy, top_z), ncol = 2),
                             radius = r, sides = 10)
    if (!is.null(trunk)) rgl::shade3d(trunk, color = "#3e2a07", alpha = 1)
    canopy <- cone3d_safe(c(cx, cy, top_z), h * 0.35, r * 3)
    if (!is.null(canopy)) rgl::shade3d(canopy, color = col_i, alpha = alpha_vals[i])
  }

  if (!is.null(export_path))
    try(rgl::snapshot3d(filename = export_path))

  cat("? Forest rendered safely - no zero-length errors.\n")
}
