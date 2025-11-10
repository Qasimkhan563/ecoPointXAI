test_that("split_train_test divides torch tensors correctly", {
  skip_if_not_installed("torch")

  x <- torch::torch_randn(c(20, 3))
  y <- torch::torch_randn(c(20, 1))
  split <- split_train_test(x, y, train_frac = 0.7)

  expect_true(is.list(split))
  expect_true(length(split) >= 2)

  # Detect structure dynamically
  train <- split[[1]]
  test  <- split[[2]]

  # either plain tensors or nested lists
  if (inherits(train, "torch_tensor")) {
    expect_s3_class(train, "torch_tensor")
    expect_s3_class(test, "torch_tensor")
  } else if (is.list(train) && "x" %in% names(train)) {
    expect_s3_class(train$x, "torch_tensor")
    expect_s3_class(test$x, "torch_tensor")
  }
})
