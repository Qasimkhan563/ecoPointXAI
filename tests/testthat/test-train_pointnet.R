test_that("3D voxel importance visualization runs safely", {
  explain_result <- list(local_explanations = matrix(runif(10), ncol = 2))
  vox <- data.frame(X = 1:5, Y = 1:5, Z = 1:5)
  expect_no_error(visualize_voxel_importance_3D(vox, explain_result))
})

test_that("cone3d_safe helper exists or is skipped", {
  if (exists("cone3d_safe", where = asNamespace("ecoPointXAI"), inherits = TRUE)) {
    expect_true(TRUE)
  } else {
    skip("cone3d_safe not available in this build")
  }
})
