test_that("create_dataloader builds iterable batches", {
  skip_if_no_pkg("torch")

  # --- Create dummy torch tensors ---
  x <- torch::torch_randn(c(20, 3))
  y <- torch::torch_randn(c(20, 1))

  # --- Build dataloader ---
  loader <- create_dataloader(x, y, batch_size = 5)
  expect_true(is.list(loader))
  expect_true(length(loader) > 0)

  # --- Fetch first batch ---
  batch1 <- get_batch(loader, 1)
  expect_true(is.list(batch1))
  expect_equal(length(batch1), 2)

  # --- Interpret returned batch flexibly (unnamed or named) ---
  bx <- if (!is.null(names(batch1))) batch1[[1]] else batch1[[1]]
  by <- if (length(batch1) > 1) batch1[[2]] else NULL

  expect_s3_class(bx, "torch_tensor")
  if (!is.null(by)) expect_s3_class(by, "torch_tensor")

  # --- Verify batch sizes ---
  expect_true(bx$size(1) <= 5)
  if (!is.null(by)) expect_equal(bx$size(1), by$size(1))
})
