#' Split tensors into training and test sets
#'
#' Randomly divides tensors into train and validation subsets.
#' @param x Feature tensor.
#' @param y Optional target tensor.
#' @param train_frac Fraction of samples to allocate to training (default 0.8).
#' @param seed Random seed for reproducibility.
#' @return A list containing train/test tensors.
#' @export
split_train_test <- function(x, y = NULL, train_frac = 0.8, seed = 42) {
  if (!inherits(x, "torch_tensor"))
    stop("x must be a torch tensor.")

  n <- as.numeric(x$size(1))
  set.seed(seed)
  idx <- sample.int(n)
  n_train <- floor(train_frac * n)
  idx_train <- idx[seq_len(n_train)]
  idx_test  <- idx[(n_train + 1):n]

  x_train <- x[idx_train, ]
  x_test  <- x[idx_test, ]

  if (!is.null(y)) {
    if (!inherits(y, "torch_tensor"))
      stop("y must be a torch tensor.")
    y_train <- y[idx_train]
    y_test  <- y[idx_test]
    return(list(x_train = x_train, y_train = y_train,
                x_test = x_test, y_test = y_test))
  } else {
    return(list(x_train = x_train, x_test = x_test))
  }
}
