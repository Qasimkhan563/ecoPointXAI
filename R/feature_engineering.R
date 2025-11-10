# ================================================================
NA
# File: R/feature_engineering.R
# Purpose: Derive 3D and ecological descriptors from voxelized LiDAR
# Pipeline stage: 3??  Feature Engineering
# Author: ecoPointXAI Team
# ================================================================

#' Extract detailed canopy-structure metrics from a normalized LAS
#'
#' Derives height-based and density metrics frequently used in forest ecology.
#'
#' @param pc LAS object (preferably ground-normalized).
#' @return A named list of metrics.
#' @export
extract_canopy_metrics <- function(pc) {
  stopifnot(inherits(pc, "LAS"))
  z <- pc@data$Z
  intensity <- pc@data$Intensity %||% rep(NA, length(z))

  list(
    max_height   = max(z, na.rm = TRUE),
    mean_height  = mean(z, na.rm = TRUE),
    sd_height    = stats::sd(z, na.rm = TRUE),
    p95_height   = stats::quantile(z, 0.95, na.rm = TRUE),
    canopy_cover = mean(z > 2, na.rm = TRUE),
    point_density = nrow(pc@data) / (diff(range(pc@data$X)) * diff(range(pc@data$Y))),
    roughness     = diff(range(z, na.rm = TRUE)),
    mean_intensity = mean(intensity, na.rm = TRUE),
    sd_intensity   = stats::sd(intensity, na.rm = TRUE)
  )
}

# ----------------------------------------------------------------
NA
# ----------------------------------------------------------------

#' Compute derived features per voxel
#'
#' Adds canopy indices and neighbor-based descriptors to voxel data.
#'
#' @param vox Data frame from [preprocess_voxelize()], with columns X, Y, Z, value.
#' @return Data frame of enriched voxel features.
#' @export
add_voxel_features <- function(vox) {
  stopifnot(all(c("X", "Y", "Z", "value") %in% names(vox)))

  vox <- vox[complete.cases(vox), ]

  # --- Normalize Z (height fraction within block) ---
  vox$z_norm <- (vox$Z - min(vox$Z)) / (max(vox$Z) - min(vox$Z))

  # --- Relative density per horizontal slice ---
  slice_density <- aggregate(value ~ Z, vox, mean)
  colnames(slice_density)[2] <- "slice_mean"
  vox <- merge(vox, slice_density, by = "Z", all.x = TRUE)

  # --- Height-weighted value (NDVI-like canopy index) ---
  vox$canopy_index <- vox$value * vox$z_norm

  # --- Neighbor density (local 3?3?3 kernel) ---
  vox$neighbor_density <- apply(
    cbind(vox$X, vox$Y, vox$Z),
    1,
    function(pt) {
      d <- sqrt((vox$X - pt[1])^2 + (vox$Y - pt[2])^2 + (vox$Z - pt[3])^2)
      mean(vox$value[d < 2 & d > 0], na.rm = TRUE)
    }
  )

  # --- Roughness proxy: local standard deviation of value ---
  vox$local_sd <- apply(
    cbind(vox$X, vox$Y, vox$Z),
    1,
    function(pt) {
      d <- sqrt((vox$X - pt[1])^2 + (vox$Y - pt[2])^2 + (vox$Z - pt[3])^2)
      stats::sd(vox$value[d < 2 & d > 0], na.rm = TRUE)
    }
  )

  return(vox)
}

# ----------------------------------------------------------------
NA
# ----------------------------------------------------------------

#' Aggregate voxel data to a raster map
#'
#' Converts voxelized 3D data (X, Y, value) into a 2D raster layer.
#'
#' @param vox Output of [preprocess_voxelize()] or [add_voxel_features()].
#' @param crs EPSG code (numeric) or [sf::st_crs] object.
#' @param field Character, which field to rasterize (default = "value").
#' @return A [terra::SpatRaster].
#' @export
aggregate_to_raster <- function(vox, crs = NULL, field = "value") {
  stopifnot(all(c("X", "Y", field) %in% names(vox)))

  # --- Handle CRS input ---
  if (!is.null(crs)) {
    if (is.numeric(crs)) crs <- sf::st_crs(crs)$wkt
    else if (inherits(crs, "crs")) crs <- crs$wkt
  }

  # --- Base raster grid ---
  r <- terra::rast(
    xmin = min(vox$X, na.rm = TRUE),
    xmax = max(vox$X, na.rm = TRUE),
    ymin = min(vox$Y, na.rm = TRUE),
    ymax = max(vox$Y, na.rm = TRUE),
    resolution = attr(vox, "res") %||% 1,
    crs = crs
  )

  # --- Convert to SpatVector and rasterize ---
  v <- terra::vect(vox, geom = c("X", "Y"), crs = crs)
  r <- terra::rasterize(v, r, field = field, fun = mean, background = NA)

  return(r)
}

# ----------------------------------------------------------------
# ?? Master function for feature-engineering pipeline
# ----------------------------------------------------------------

#' Run full feature-engineering workflow
#'
#' Wrapper that calls canopy metrics, voxel features, and raster summaries.
#'
#' @param las LAS object (normalized).
#' @param vox Data frame from [preprocess_voxelize()].
#' @param crs EPSG or [sf::st_crs].
#' @return List with canopy metrics, enriched voxel data, and optional rasters.
#' @export
feature_engineering_pipeline <- function(las, vox, crs = NULL) {
NA
  canopy <- extract_canopy_metrics(las)

NA
  vox_features <- add_voxel_features(vox)

NA
  r_value <- aggregate_to_raster(vox_features, crs, field = "value")
  r_canopy <- aggregate_to_raster(vox_features, crs, field = "canopy_index")

  return(list(
    canopy_metrics = canopy,
    voxel_features = vox_features,
    raster_value = r_value,
    raster_canopy = r_canopy
  ))
}
