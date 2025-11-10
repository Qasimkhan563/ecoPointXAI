test_that("Feature engineering pipeline derives canopy & voxel metrics", {
  skip_if_no_pkg("lidR")
  skip_if_no_pkg("terra")

  las <- lidR::LAS(data.frame(
    X = runif(50, 0, 10),
    Y = runif(50, 0, 10),
    Z = runif(50, 1, 10)
  ))

  vox <- data.frame(
    X = runif(100, 0, 10),
    Y = runif(100, 0, 10),
    Z = runif(100, 0, 10),
    value = runif(100)
  )

  expect_silent({
    fe <- suppressMessages(feature_engineering_pipeline(las, vox, crs = 3763))
  })
  expect_true(is.list(fe))
  expect_true("canopy_metrics" %in% names(fe))
})
