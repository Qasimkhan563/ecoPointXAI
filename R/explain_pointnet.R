# ================================================================
NA
# File: R/explain_pointnet.R
# Purpose: Compute SHAP- and gradient-based explanations for PointNet
# Stage: 8??  Model Explanation
# ================================================================

#' Explain a trained PointNet model using SHAP or gradient-based methods
#'
#' Generates voxel-level and feature-level explanations for trained models.
#'
#' @param model A trained PointNet model (see ecoPointXAI::train_pointnet()).
#' @param voxel_data A data frame or torch tensor of voxel features (e.g., X, Y, Z, value)
#' @param method One of "shap" (model-agnostic) or "gradients" (model-specific)
#' @param n_samples Number of random samples for SHAP (default = 500)
#' @param device Computation device: "cpu" or "cuda" (auto-detected)
#'
#' @return A list containing:
#' \itemize{
#'   \item feature_importance - tibble of average importance per feature
#'   \item local_explanations - matrix of per-voxel attributions (for gradients)
#'   \item shap_values - data frame of Shapley values (for SHAP)
#' }
#' @export
explain_pointnet <- function(model,
                             voxel_data,
                             method = c("shap", "gradients"),
                             n_samples = 500,
                             device = if (torch::cuda_is_available()) "cuda" else "cpu") {

  method <- match.arg(method)
  model$eval()
  model <- model$to(device = device)

  # ----------------------------------------------------------
  # ? Prepare voxel data (supports torch_tensor or data.frame)
  # ----------------------------------------------------------
  if (inherits(voxel_data, "torch_tensor")) {
    x <- voxel_data$to(device = device)
    x_df <- as.data.frame(as.matrix(x$to(device = "cpu")))
    feature_names <- paste0("F", seq_len(ncol(x_df)))  # generic feature names
  } else if (is.data.frame(voxel_data)) {
    x_df <- voxel_data
    x <- torch::torch_tensor(as.matrix(voxel_data), dtype = torch::torch_float())
    feature_names <- colnames(voxel_data)
  } else {
    stop("voxel_data must be either a data.frame or torch_tensor.")
  }

  # ==========================================================
NA
  # ==========================================================
  if (method == "shap") {
NA

    x_df <- as.data.frame(as.matrix(x$to(device = "cpu")))

    # Randomly subsample for efficiency
    if (nrow(x_df) > n_samples)
      x_df <- x_df[sample(seq_len(nrow(x_df)), n_samples), , drop = FALSE]

    # Prediction function
    predict_fn <- function(newdata) {
      new_tensor <- torch::torch_tensor(as.matrix(newdata), dtype = torch::torch_float())
      preds <- model(new_tensor$to(device = device))
      as.numeric(preds$to(device = "cpu"))
    }

    # ? Correct usage for iml
    predictor <- iml::Predictor$new(
      model = NULL,                      # we pass function instead
      data = x_df,
      y = predict_fn(x_df),
      predict.function = predict_fn
    )

    shapley <- iml::Shapley$new(
      predictor,
      x.interest = x_df
    )

    shap_values <- shapley$results

    feature_importance <- shap_values |>
      dplyr::group_by(feature) |>
      dplyr::summarise(mean_abs_phi = mean(abs(phi), na.rm = TRUE)) |>
      dplyr::arrange(desc(mean_abs_phi))

    cat("? SHAP explanations computed successfully.\n")

    return(list(
      feature_importance = feature_importance,
      shap_values = shap_values,
      local_explanations = NULL
    ))
  }

  # ==========================================================
NA
  # ==========================================================
  if (method == "gradients") {
    cat("?? Computing gradient-based feature attributions ...\n")

    x$requires_grad_(TRUE)
    preds <- model(x)
    preds$backward(torch::torch_ones_like(preds))

    grads <- x$grad$detach()$to(device = "cpu")
    grad_matrix <- as.matrix(grads)

    feature_importance <- tibble::tibble(
      feature = feature_names,
      mean_grad = colMeans(abs(grad_matrix), na.rm = TRUE)
    ) |>
      dplyr::arrange(desc(mean_grad))

    cat("? Gradient-based explanations computed successfully.\n")

    return(list(
      feature_importance = feature_importance,
      local_explanations = grad_matrix,
      shap_values = NULL
    ))
  }
}
