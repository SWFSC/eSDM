# definition from https://stats.stackexchange.com/questions/10289/whats-the-difference-between-normalization-and-standardization
esdm_normalize <- function(x) {
  num <- (x - min(x, na.rm = TRUE))
  denom <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  num / denom
}

esdm_rmse <- function(x, y) {
  sqrt(mean((x - y) ^ 2, na.rm = TRUE))
}
