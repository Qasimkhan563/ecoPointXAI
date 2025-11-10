test_that("interpret_ecological_drivers computes and correlates metrics", {
  shap_data <- data.frame(
    X_grid = 1:15,
    Y_grid = 1:15,
    importance = runif(15)
  )

  eco_metrics <- list(
    grid_metrics = data.frame(
      X_grid = 1:15,
      Y_grid = 1:15,
      mean_height = rnorm(15),
      canopy_cover = runif(15)
    )
  )
  class(eco_metrics) <- "eco_metrics"

  res <- interpret_ecological_drivers(shap_data, eco_metrics, top_n = 5, plot = FALSE)
  expect_true(is.list(res))
  expect_true("correlations" %in% names(res))
  expect_true(is.data.frame(res$correlations))
})
