###############################################################################
# Calculate root mean squared error
esdm_rmse <- function(x, y) {
  sqrt(mean((x - y) ^ 2, na.rm = TRUE))
}


###############################################################################
# Weighted mean function for eSDM
#   Different from stats::weighted.mean in that NA values of w can also be ignored
esdm_weighted_mean <- function(x, w, ...) {
  ### Inputs:
  # x: numeric vector of values for which to calculate the weighted mean
  # w: numeric vector of weights
  # ...: for passing na.rm argument

  stopifnot(
    inherits(x, c("numeric", "integer")),
    inherits(w, c("numeric", "integer"))
  )

  lst <- list(...)
  z.lgl <- if ("na.rm" %in% names(lst)) lst$na.rm else TRUE

  if (z.lgl) {
    idx <- which(!is.na(x) & !is.na(w))
    x <- x[idx]
    w <- w[idx]
  } else {
    idx <- seq_along(x)
  }

  if (length(idx) == 0) {
    NA

  } else {
    w <- w / sum(w, na.rm = z.lgl)
    sum(x * w, na.rm = z.lgl)
  }
}

# esdm_weighted_mean(c(NA, 2, 3), c(1, 2, 3), na.rm = TRUE)
# esdm_weighted_mean(c(NA, 2, 3), c(1, 2, 3), na.rm = FALSE)
# esdm_weighted_mean(c(4, 2, 3), c(1, 2, 3), na.rm = TRUE)
# esdm_weighted_mean(c(4, 2, 3), c(1, 2, 3), na.rm = FALSE)
# esdm_weighted_mean(c(4, 2, 3), c(1, 2, NA), na.rm = TRUE)
# esdm_weighted_mean(c(4, 2, 3), c(1, 2, NA), na.rm = FALSE)


###############################################################################
# Weighted variance function for eSDM

#------------------------------------------------------------------------------
### Calculate among-model variance from weights and values
###   used to calculate weighted mean
esdm_weighted_var_amv <- function(x, x.mean, w, ...) {
  ### Inputs:
  # x: numeric vector of values that were used to calculate weighted mean
  # x.mean: weighted mean of values in x, calculated using w
  # w: numeric vector of weights used to calculate weighted mean
  # ...: for passing na.rm argument

  stopifnot(
    inherits(x, c("numeric", "integer")),
    inherits(x.mean, c("numeric", "integer")),
    inherits(w, c("numeric", "integer"))
  )

  lst <- list(...)
  z.lgl <- if ("na.rm" %in% names(lst)) lst$na.rm else TRUE

  if (z.lgl) {
    idx <- which(!is.na(x) & !is.na(w))
    x <- x[idx]
    w <- w[idx]
  } else {
    idx <- seq_along(x)
  }

  if (length(idx) == 0) {
    NA

  } else {
    w <- w / sum(w, na.rm = z.lgl)
    sum(w * (x - x.mean)^2, na.rm = z.lgl)
  }
}


#------------------------------------------------------------------------------
### Calculate within-model variance from weights and variances of
###   values used to calculate weighted mean
esdm_weighted_var_wmv <- function(x.var, w, ...) {
  ### Inputs:
  # x: numeric vector of variances of values used to calculate weighted mean
  # w: numeric vector of weights used to calculate weighted mean
  # ...: for passing na.rm argument

  stopifnot(
    inherits(x.var, c("numeric", "integer")),
    inherits(w, c("numeric", "integer"))
  )

  lst <- list(...)
  z.lgl <- if ("na.rm" %in% names(lst)) lst$na.rm else TRUE

  if (z.lgl) {
    idx <- which(!is.na(x.var) & !is.na(w))
    x.var <- x.var[idx]
    w <- w[idx]
  } else {
    idx <- seq_along(x.var)
  }

  if (length(idx) == 0) {
    NA

  } else {
    w <- w / sum(w, na.rm = z.lgl)
    sum(w^2 * x.var, na.rm = z.lgl)
  }
}

###############################################################################
