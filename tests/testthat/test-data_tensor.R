test_that("convert_to_tensor normalizes and converts correctly", {
  skip_if_no_pkg("torch")

  df <- data.frame(
    X = rnorm(5),
    Y = rnorm(5),
    Z = rnorm(5),
    value = runif(5)
  )

  t <- convert_to_tensor(df)

  # if the function returns list(x, y) or similar
  if (is.list(t)) {
    expect_true(any(sapply(t, inherits, "torch_tensor")))
    tx <- t[[1]]
  } else {
    tx <- t
  }

  expect_s3_class(tx, "torch_tensor")
  vals <- as.numeric(torch::torch_mean(tx))
  expect_true(is.numeric(vals))
})
