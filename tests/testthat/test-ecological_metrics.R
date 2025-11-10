test_that("Ecological metrics functions run end-to-end", {
  # Core dependency check
  skip_if_no_pkg("terra")

  # Skip if visualization packages are missing
  skip_if_not_installed("rgl")
  skip_if_not_installed("rayshader")
  skip_if_not_installed("viridisLite")

  # Create demo voxel data
  vox <- data.frame(
    X = runif(50, 0, 5),
    Y = runif(50, 0, 5),
    Z = runif(50, 0, 5),
    value = runif(50)
  )

  # Compute ecological metrics
  metrics <- compute_ecological_metrics(vox)
  expect_true(is.list(metrics))
  expect_true("grid_metrics" %in% names(metrics))

  # Visualization and export checks (no crash expected)
  visualize_ecological_metrics(metrics)
  map_ecological_metrics_gl(metrics, metric_name = "mean_height", sample_frac = 0.05)

  suppressWarnings({
    map_ecological_metrics_terrain(metrics, save_path = tempfile(fileext = ".png"))
  })

  # Export metrics safely
  tmp <- tempfile()
  export_ecological_metrics(metrics, out_dir = tmp)
  expect_true(dir.exists(tmp))
})
