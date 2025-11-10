#' Normalize ground height and voxelize a LAS cloud
#'
#' Performs ground-surface normalization using a TIN model and converts
#' the cloud into a regular voxel grid summarizing point density or height stats.
#'
#' @param pc A [lidR::LAS] object.
#' @param res Numeric; voxel resolution in map units.
#' @param stat Character; summary statistic: `"count"`, `"mean"`, `"max"`, `"sd"`.
#' @return A `data.frame` with columns X, Y, Z, value.
#' @export
preprocess_voxelize <- function(pc, res = 0.25, stat = "count") {
  stopifnot(inherits(pc,"LAS"))
  stopifnot(res > 0)

  message("? Normalizing ground ...")
  pc_norm <- lidR::normalize_height(pc, lidR::tin())

  message("? Computing voxel metrics ...")
  metric_fun <- switch(
    stat,
    count = ~length(Z),
    mean  = ~mean(Z),
    max   = ~max(Z),
    sd    = ~sd(Z),
    stop("Unknown stat")
  )

  vox <- lidR::voxel_metrics(pc_norm, metric_fun, res = res)
  df <- as.data.frame(vox)
  names(df)[4] <- "value"
  attr(df, "res") <- res
  attr(df, "stat") <- stat
  return(df)
}
