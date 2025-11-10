#' Convert voxel data to torch tensors
#'
#' Turns a voxelized data frame (X, Y, Z, value) into input and target tensors.
#' @param vox data frame returned by [preprocess_voxelize()].
#' @param target Optional column name to use as target variable (for regression/classification).
#'   If `NULL`, only features are returned.
#' @param normalize Logical; if TRUE (default), columns are z-scaled.
#' @return A list with components:
#'   * `x` - input [torch::torch_tensor]
#'   * `y` - target tensor (if `target` supplied)
#' @examples
#' \dontrun{
#' tns <- convert_to_tensor(vox)
#' }
#' @export
convert_to_tensor <- function(vox, target = NULL, normalize = TRUE) {
  stopifnot(is.data.frame(vox))
  req_cols <- c("X", "Y", "Z", "value")
  if (!all(req_cols %in% names(vox)))
    stop("Voxel data must include X, Y, Z, and value columns.")

  df <- vox[, req_cols, drop = FALSE]
  if (normalize) {
    df <- as.data.frame(scale(df))
  }

  x <- torch::torch_tensor(as.matrix(df), dtype = torch::torch_float())

  if (!is.null(target)) {
    stopifnot(target %in% names(vox))
    y <- torch::torch_tensor(vox[[target]])
    return(list(x = x, y = y))
  } else {
    return(list(x = x))
  }
}
