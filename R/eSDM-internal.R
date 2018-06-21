# Sort x by col1 and then (if applicable) col2
data_sort <- function(x, col1 = 1, col2 = NA) {
  if (!is.na(col2)) x <- x[order(x[, col2]), ]
  x[order(x[, col1]), ]
}


# Determine whether all values in x are equal;
# From Hadley on stack overflow
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}


# Get last n element(s) from string x
# From https://stackoverflow.com/questions/7963898
substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}


# Determine which elements of the x are one of invalid
# Invalid elements are "N/A", "n/a", "na", "NaN", or ""
na_which <- function(x) {
  na.char <- c("N/A", "n/a", "na", "NaN", "")

  na.idx <- suppressWarnings(
    c(which(is.na(x)), which(is.nan(x)), which(x %in% na.char), which(x < 0))
  )

  if (length(na.idx) == 0) NA else sort(unique(na.idx))
}


# Generate message reporting length of x
# This message was built to refer to prediction values
na_pred_message <- function(x) {
  if (anyNA(x)) {
    "No prediction values were classified as NA"
  } else {
    len.x <- length(x)
    ifelse(len.x == 1,
           paste(len.x, "prediction value was classified as NA"),
           paste(len.x, "prediction values were classified as NA"))
  }
}


# Generate message reporting length of x
# This message was built to refer to weight values, including if any non-NA
#   prediction values corresponded to NA weight values
na_weight_message <- function(x, y) {
  len.x <- length(x)
  if (anyNA(x)) {
    "No weight values were classified as NA"

  } else if (!all(x %in% y)) {
    paste0(
      ifelse(len.x == 1,
             paste(len.x, "weight value was classified as NA"),
             paste(len.x, "weight values were classified as NA")),
      "<br/>Some non-NA prediction values have NA weight values"
    )

  } else {
    paste0(
      ifelse(len.x == 1,
             paste(len.x, "weight value was classified as NA"),
             paste(len.x, "weight values were classified as NA")),
      "<br/>No non-NA prediction values have NA weight values"
    )
  }
}


# Normalize vector of model predictions, 'x'
normalize <- function(x) {
  num <- (x - min(x, na.rm = TRUE))
  denom <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  num / denom
}


# Round 'x' to nearest 'base' value
mround <- function(x, base, floor.use = FALSE, ceiling.use = FALSE) {
  if (floor.use) {
    base * floor(x / base)

  } else if (ceiling.use) {
    base * ceiling(x / base)

  } else {
    base * round(x / base)
  }
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
