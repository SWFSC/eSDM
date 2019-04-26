# Calculate root mean squared error
esdm_rmse <- function(x, y) {
  sqrt(mean((x - y) ^ 2, na.rm = TRUE))
}
