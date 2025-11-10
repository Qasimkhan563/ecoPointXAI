#' Create a mini-batch data loader for voxel tensors
#'
#' Lightweight and Windows-safe mini-batch loader.
#' Works on all R-torch versions without threading deadlocks.
#'
#' @param x Feature tensor (from [convert_to_tensor()]).
#' @param y Optional target tensor.
#' @param batch_size Number of samples per batch.
#' @param shuffle Logical; shuffle order each epoch (default = TRUE).
#' @param num_workers Number of parallel workers (default = 1).
#' @return A dataloader list containing tensors \code{x} and \code{y} for model training.
#' @export
create_dataloader <- function(x, y = NULL, batch_size = 64, shuffle = TRUE, num_workers = 1) {
  if (!inherits(x, "torch_tensor"))
    stop("x must be a torch tensor.")
  if (!is.null(y) && !inherits(y, "torch_tensor"))
    stop("y must be a torch tensor if provided.")

  n <- as.numeric(x$size(1))
  idx <- seq_len(n)
  if (shuffle) idx <- sample(idx)

  batches <- list()
  n_batches <- ceiling(n / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n)

    sel <- idx[start_idx:end_idx]

    # ?? handle last batch gracefully
    if (length(sel) == 0) next

    x_batch <- x[sel, , drop = FALSE]
    if (is.null(y)) {
      batches[[i]] <- list(x_batch)
    } else {
      y_batch <- y[sel]
      batches[[i]] <- list(x_batch, y_batch)
    }
  }

  cat(sprintf("? Created dataloader with %d batches ? %d max samples each\n",
              length(batches), batch_size))
  class(batches) <- c("eco_dataloader", class(batches))
  batches
}

#' Fetch the next batch from a loader list
#'
#' @param loader Object created by [create_dataloader()].
#' @param i Batch index (defaults to 1).
#' @return A batch (list of tensors).
#' @export
get_batch <- function(loader, i = 1L) {
  stopifnot(inherits(loader, "eco_dataloader"))
  if (i > length(loader)) return(NULL)
  loader[[i]]
}
