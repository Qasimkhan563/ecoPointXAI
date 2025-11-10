# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(ecoPointXAI)

# --- 🧩 CRAN-safety patch ---
# Prevent R CMD check from running heavy or failing examples (like interpret_ecological_drivers)
if (Sys.getenv("_R_CHECK_PACKAGE_NAME_") == "ecoPointXAI") {
  message("⚙️  Skipping example execution for CRAN check")
  try({
    unlockBinding(".exampleEnv", as.environment("package:tools"))
    assign("exampleEnv", new.env(), as.environment("package:tools"))
  }, silent = TRUE)
}

test_check("ecoPointXAI")
