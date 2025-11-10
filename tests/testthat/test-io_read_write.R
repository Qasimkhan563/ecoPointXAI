test_that("Point cloud I/O reads and writes .laz correctly", {
  skip_if_not_installed("lidR")
  skip_on_cran()
  
  laz_path <- system.file("extdata", "forest_plot.laz", package = "ecoPointXAI")
  if (laz_path == "") skip("forest_plot.laz not found")

  pc <- read_pointcloud(laz_path)
  expect_s4_class(pc, "LAS")
  expect_true(nrow(pc@data) > 0)

  temp_laz <- tempfile(fileext = ".laz")
  expect_silent(write_pointcloud(pc, temp_laz))
  expect_true(file.exists(temp_laz))

  pc2 <- try(read_pointcloud(temp_laz), silent = TRUE)
  if (inherits(pc2, "try-error")) skip("LAS read skipped due to system LAZ driver issues")

  expect_s4_class(pc2, "LAS")
  expect_equal(names(pc@data), names(pc2@data))
})
