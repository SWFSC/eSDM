# definition from https://stats.stackexchange.com/questions/10289/whats-the-difference-between-normalization-and-standardization
esdm_normalize <- function(x) {
  num <- (x - min(x, na.rm = TRUE))
  denom <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  num / denom
}


# Calculate break points for density intervals
# Break points are at: 2%, 5%, 10%, 15%, 20%, 25%, 30%, 35%, 40%
breaks_calc <- function(x) {
  breaks <- rev(c(0.02, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40))
  # if (any(is.na(sp.data))) warning("NA's removed")
  # if (any(sp.data == 0, na.rm = TRUE)) warning("Densities contain 0's")
  # if (any(sp.data < 0, na.rm = TRUE)) warning("Densities contain values < 0")

  x <- x[!is.na(x)]
  x <- sort(x, decreasing = TRUE)

  data.len <- length(x)
  data.max <- max(x)
  data.min <- min(x)

  data.breaks.mid <- sapply(breaks, function(i) {
    x[ceiling(i * data.len)]
  })

  c(data.min, data.breaks.mid, data.max)
}
