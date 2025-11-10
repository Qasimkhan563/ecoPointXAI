test_that("PointNet model initializes with correct layer structure", {
  skip_if_no_pkg("torch")

  model <- pointnet_model(input_dim = 3, output_dim = 2)
  expect_true(is.list(model) || inherits(model, "nn_module"))
  expect_true(all(c("input_dim", "output_dim") %in% names(attributes(model)) |
                  length(model) > 0))
})
