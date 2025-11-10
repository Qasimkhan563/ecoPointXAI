#' Define a simple PointNet-style neural network
#'
#' @param input_dim Number of input features (e.g. 3 for X,Y,Z or 4 for X,Y,Z,value)
#' @param output_dim Number of target classes (for regression use 1)
#' @return A torch nn_module
#' @export
pointnet_model <- function(input_dim = 3, output_dim = 2) {

  torch::nn_module(
    "PointNetSimple",

    initialize = function() {
      self$fc1 <- torch::nn_linear(input_dim, 64)
      self$bn1 <- torch::nn_batch_norm1d(64)
      self$fc2 <- torch::nn_linear(64, 128)
      self$bn2 <- torch::nn_batch_norm1d(128)
      self$fc3 <- torch::nn_linear(128, 256)
      self$bn3 <- torch::nn_batch_norm1d(256)
      self$fc_out <- torch::nn_linear(256, output_dim)
      self$relu <- torch::nn_relu()
    },

    forward = function(x) {
      x <- self$relu(self$bn1(self$fc1(x)))
      x <- self$relu(self$bn2(self$fc2(x)))
      x <- self$relu(self$bn3(self$fc3(x)))
      x <- self$fc_out(x)
      return(x)
    }
  )
}
