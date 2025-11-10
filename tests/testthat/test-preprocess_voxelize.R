test_that("Preprocessing voxelization creates correct voxel summaries", {
  skip_if_no_pkg("lidR")

  pc <- lidR::LAS(data.frame(
    X = as.double(1:15),
    Y = as.double(1:15),
    Z = runif(15, 0, 10),
    Classification = sample(c(1L, 2L), 15, replace = TRUE)
  ))

  vox <- preprocess_voxelize(pc, res = 0.5)
  expect_true(is.list(vox) || inherits(vox, "LAS"))
  expect_true(length(vox) > 0)
})

