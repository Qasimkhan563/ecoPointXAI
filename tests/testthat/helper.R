# ------------------------------------------------------------
# Common helper utilities for ecoPointXAI tests
# ------------------------------------------------------------

skip_if_no_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste("Package", pkg, "not installed"))
  }
}

# small synthetic voxel dataset for repeated use
vox_small <- data.frame(
  X = runif(100, 0, 10),
  Y = runif(100, 0, 10),
  Z = runif(100, 0, 10),
  value = runif(100)
)
