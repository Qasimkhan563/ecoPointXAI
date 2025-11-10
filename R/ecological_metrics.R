# ================================================================
NA
# ================================================================

#' Compute ecological canopy metrics from voxel data
#'
#' Extracts canopy height, variability, cover density, and complexity metrics.
#'
#' @param vox_df Data frame with X, Y, Z, value (e.g., voxel intensity)
#' @param grid_size Numeric - grid cell size (default = 1)
#' @return A list with `overall` summary and `grid_metrics` table
#' @export
compute_ecological_metrics <- function(vox_df, grid_size = 1) {
  if (!all(c("X", "Y", "Z", "value") %in% colnames(vox_df))) {
    stop("vox_df must have X, Y, Z, and value columns.")
  }

  vox_df$X_grid <- floor(vox_df$X / grid_size)
  vox_df$Y_grid <- floor(vox_df$Y / grid_size)

  metrics <- vox_df |>
    dplyr::group_by(X_grid, Y_grid) |>
    dplyr::summarise(
      mean_height = mean(Z, na.rm = TRUE),
      max_height = max(Z, na.rm = TRUE),
      sd_height = sd(Z, na.rm = TRUE),
      canopy_cover = mean(value > 0, na.rm = TRUE),
      canopy_complexity = sd(value, na.rm = TRUE) /
        (mean(value, na.rm = TRUE) + 1e-6),
      .groups = "drop"
    )

  summary <- list(
    overall = metrics |>
      dplyr::summarise(
        mean_height = mean(mean_height, na.rm = TRUE),
        p95_height = quantile(max_height, 0.95, na.rm = TRUE),
        canopy_cover = mean(canopy_cover, na.rm = TRUE),
        mean_complexity = mean(canopy_complexity, na.rm = TRUE)
      ),
    grid_metrics = metrics
  )

  class(summary) <- c("eco_metrics", class(summary))
  return(summary)
}

# ================================================================
NA
# ================================================================
visualize_ecological_metrics <- function(metrics) {
  stopifnot(inherits(metrics, "eco_metrics"))
  df <- metrics$overall |> as.data.frame()

  # Bar Chart
  bar_plot <- plotly::plot_ly(
    x = names(df),
    y = as.numeric(df[1, ]),
    type = "bar",
    marker = list(color = viridisLite::viridis(ncol(df))),
    hoverinfo = "x+y"
  ) |>
    plotly::layout(
      title = "Ecological Metrics Summary",
      xaxis = list(title = ""),
      yaxis = list(title = "Value"),
      margin = list(l = 50, r = 30, b = 60, t = 60)
    )

  # Radar Chart
  radar_df <- data.frame(metric = names(df), value = as.numeric(df[1, ]))
  radar_plot <- plotly::plot_ly(
    type = "scatterpolar",
    r = radar_df$value,
    theta = radar_df$metric,
    fill = "toself",
    line = list(color = "#1b9e77")
  ) |>
    plotly::layout(
      title = "Structural Diversity Radar",
      polar = list(radialaxis = list(
        visible = TRUE,
        range = c(0, max(radar_df$value, na.rm = TRUE))
      ))
    )

  htmltools::browsable(htmltools::tagList(bar_plot, radar_plot))
}

# ================================================================
NA
# ================================================================
map_ecological_metrics_gl <- function(metrics,
                                      metric_name = "mean_height",
                                      sample_frac = 0.05) {
  stopifnot(inherits(metrics, "eco_metrics"))
  df <- metrics$grid_metrics

  if (nrow(df) > 50000)
    df <- df[sample(seq_len(nrow(df)), 50000), ]

  sf_obj <- sf::st_as_sf(df, coords = c("X_grid", "Y_grid"), crs = 3763)
  sf_obj <- sf::st_transform(sf_obj, 4326)
  pal <- leaflet::colorNumeric("viridis", df[[metric_name]], na.color = "transparent")

  m <- leaflet::leaflet(sf_obj) |>
    leaflet::addProviderTiles("Esri.WorldImagery") |>
    leaflet::addCircleMarkers(
      radius = 3,
      color = ~pal(df[[metric_name]]),
      opacity = 0.8,
      fillOpacity = 0.8
    ) |>
    leaflet::addLegend(
      "bottomright",
      pal = pal,
      values = df[[metric_name]],
      title = paste(metric_name, "(m)"),
      opacity = 1
    )

  dir.create("outputs", showWarnings = FALSE)
  out_file <- file.path("outputs", paste0("ecoPointXAI_map_", metric_name, ".html"))
  htmlwidgets::saveWidget(m, out_file, selfcontained = TRUE)
  utils::browseURL(normalizePath(out_file))
  cat("? Saved 2D interactive map ?", out_file, "\n")
  invisible(m)
}

# ================================================================
NA
# ================================================================
export_ecological_metrics <- function(metrics, out_dir = "outputs/metrics") {
  stopifnot(inherits(metrics, "eco_metrics"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(metrics$grid_metrics, file.path(out_dir, "grid_metrics.csv"))
  readr::write_csv(metrics$overall, file.path(out_dir, "summary_metrics.csv"))

  htmltools::save_html(
    visualize_ecological_metrics(metrics),
    file = file.path(out_dir, "ecological_summary.html")
  )
  message("? Metrics exported to ", normalizePath(out_dir))
  invisible(metrics)
}

# ================================================================
NA
# ================================================================
map_ecological_metrics_terrain <- function(metrics,
                                           z_metric = "mean_height",
                                           color_metric = "canopy_complexity",
                                           exaggeration = 20,
                                           sample_frac = 0.05,
                                           save_path = "outputs/ecoPointXAI_terrain.png") {
  stopifnot(inherits(metrics, "eco_metrics"))

  if (!requireNamespace("rgl", quietly = TRUE) ||
      !requireNamespace("rayshader", quietly = TRUE) ||
      !requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Install packages: install.packages(c('rgl','rayshader','viridisLite'))")
  }

  df <- metrics$grid_metrics
  df <- df[is.finite(df[[z_metric]]) & is.finite(df[[color_metric]]), ]

  # Downsample for speed
  if (nrow(df) > 100000) {
    message("? Downsampling to ", round(sample_frac * 100), "% of points...")
    df <- dplyr::sample_frac(df, sample_frac)
  }

  # Fill complete grid
  x_seq <- sort(unique(df$X_grid))
  y_seq <- sort(unique(df$Y_grid))
  grid_df <- tidyr::expand_grid(X_grid = x_seq, Y_grid = y_seq) |>
    dplyr::left_join(df, by = c("X_grid", "Y_grid"))

  # Fill NAs with mean
  grid_df[[z_metric]][is.na(grid_df[[z_metric]])] <-
    mean(df[[z_metric]], na.rm = TRUE)
  grid_df[[color_metric]][is.na(grid_df[[color_metric]])] <-
    mean(df[[color_metric]], na.rm = TRUE)

  # Convert to matrices
  nx <- length(x_seq)
  ny <- length(y_seq)
  mat <- matrix(grid_df[[z_metric]], nrow = nx, ncol = ny)
  colmat <- matrix(grid_df[[color_metric]], nrow = nx, ncol = ny)

  # Normalize & rescale height
  mat_range <- range(mat, na.rm = TRUE)
  z_norm <- (mat - mat_range[1]) / diff(mat_range)
  mat <- z_norm * exaggeration * 100  # amplify terrain shape

  # Normalize color
  col_range <- range(colmat, na.rm = TRUE)
  colmat <- (colmat - col_range[1]) / diff(col_range)

  # Generate vivid magma colors
  cols <- viridisLite::magma(256)
  color_idx <- as.integer(colmat * 255) + 1
  color_rgb <- cols[pmin(pmax(color_idx, 1), 256)]

  rgl::close3d()

  mat |>
    rayshader::height_shade(texture = cols) |>
    rayshader::add_shadow(
      rayshader::ambient_shade(mat, zscale = exaggeration),
      0.6
    ) |>
    rayshader::add_shadow(
      rayshader::ray_shade(mat, zscale = exaggeration, sunaltitude = 35),
      0.5
    ) |>
    rayshader::plot_3d(
      heightmap = mat,
      zscale = exaggeration,
      solid = TRUE,
      solidcolor = "gray20",
      shadow = TRUE,
      water = FALSE,
      windowsize = c(1000, 800),
      baseshape = "rectangle",
      background = "black",
      theta = 45, phi = 45, zoom = 0.8, fov = 60
    )

  rayshader::render_snapshot(save_path)
  cat("? 3D terrain saved ?", normalizePath(save_path), "\n")
}
