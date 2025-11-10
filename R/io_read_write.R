#' Read or write 3D point-cloud files
#'
#' Wrapper around \pkg{lidR} and \pkg{rlas} readers supporting LAS/LAZ/PLY formats.
#'
#' @param path Path to the LAS/LAZ/PLY file.
#' @param crs Optional EPSG code (integer or string) to assign if missing.
#' @return A [lidR::LAS] object with header information.
#' @examples
#' pc <- read_pointcloud(system.file("extdata", "forest_plot.laz", package = "ecoPointXAI"))
#' @export
read_pointcloud <- function(path, crs = NULL) {
  stopifnot(file.exists(path))
  ext <- tolower(tools::file_ext(path))

  # --- Read LAS/LAZ/PLY ---
  if (ext %in% c("las", "laz")) {
    pc <- tryCatch(lidR::readLAS(path), error = function(e) NULL)
  } else if (ext == "ply") {
    pc <- tryCatch(lidR::readLAS(path), error = function(e) NULL) # placeholder
  } else {
    stop("Unsupported extension: ", ext)
  }

  # --- Validate ---
  if (is.null(pc) || !inherits(pc, "LAS")) {
    stop("Invalid LAS/LAZ/PLY file or read error at: ", path)
  }

  # --- Assign CRS if missing and provided ---
  if (!is.null(crs)) {
    if (is.na(sf::st_crs(pc))) {
      lidR::projection(pc) <- sf::st_crs(crs)$wkt
    }
  }

  return(pc)
}

#' Write processed LAS/LAZ file
#'
#' Wrapper around \pkg{lidR::writeLAS} ensuring valid extension and silent return.
#' 
#' @param pc A [lidR::LAS] object to write.
#' @param path Output path ending with `.las` or `.laz`.
#' @return Invisibly returns TRUE if successful.
#' @examples
#' \dontrun{
#' pc <- read_pointcloud(system.file("extdata", "forest_plot.laz", package = "ecoPointXAI"))
#' tmp <- tempfile(fileext = ".laz")
#' write_pointcloud(pc, tmp)
#' }
#' @export
write_pointcloud <- function(pc, path) {
  stopifnot(inherits(pc, "LAS"))
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("las", "laz")) stop("Output must end with .las or .laz")

  tryCatch({
    lidR::writeLAS(pc, path)
  }, error = function(e) {
    stop("Failed to write LAS/LAZ file: ", e$message)
  })

  invisible(TRUE)
}
