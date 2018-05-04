#' @title Base Frequencies
#' @description Load csv file from given shiny file input
#'
#' @param col1 integer; the primary column to sort by
#' @param col2 integer; the secondary column to sort by
#'
#' @return Sorted data.frame
#'
#' @examples
#'
#'
#' @export

data.sort <- function(x, col1 = 1, col2 = NA) {
  if (!is.na(col2)) x <- x[order(x[, col2]), ]

  x[order(x[, col1]), ]
}


#' ### Determine whether all values in x are equal, aka have a zero range
#' From Hadley Wickham
#' @export

zero.range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}


#' ## Get last n element from string x
#' From https://stackoverflow.com/questions/7963898
#' @export
substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}


#' Determine which elements of the x are
#' one of "N/A", "n/a", "na", "NaN", or ""
#' @export

na.which <- function(x) {
  na.char <- c("N/A", "n/a", "na", "NaN", "")

  na.idx <- suppressWarnings(c(which(is.na(x)),
                               which(is.nan(x)),
                               which(x %in% na.char),
                               which(x < 0)))
  na.idx <- sort(unique(na.idx))
  if (length(na.idx) == 0) na.idx <- NA

  return(na.idx)
}


#' ## Generate message reporting length of x
#' This message was built to refer to prediction values
#' @export

na.which.message <- function(x) {
  if (anyNA(x)) {
    na.len <- "No prediction values were classified as NA"
  } else {
    len.x <- length(x)
    na.len <- ifelse(len.x == 1,
                     paste(len.x, "prediction value was classified as NA"),
                     paste(len.x, "prediction values were classified as NA"))
  }
}


#' ## Normalize vector of model predictions, 'x'
#' @export

normalize <- function(x) {
  num <- (x - min(x, na.rm = TRUE))
  denom <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

  return (num / denom)
}


#' ## Rescale spdf.orig columns cols.data to data in new.abundances
#' @export

models.rescale <- function(spdf.list, abund.new) {
  spdf.list.rescaled <- lapply(spdf.list, function(s) {
    abund.orig <- model.abundance(s, cols.data = "Pred.overlaid")
    frac <- abund.orig / abund.new
    s$Pred.overlaid <- s$Pred.overlaid / frac

    s
  })

  return(spdf.list.rescaled)
}


#------------------------------------------------------------------------------
#' ## Round 'x' to nearest 'base' value
#' @export

mround <- function(x, base, floor.use = FALSE, ceiling.use = FALSE) {
  if (floor.use) {
    base * floor(x / base)
  } else if (ceiling.use) {
    base * ceiling(x / base)
  } else {
    base * round(x / base)
  }
}


#' Title
#'
#' Calculate break points for density intervals
#' Breaks at top 2%, 5%, 10%, 15%, 20%, 25%, 30%, 35%, 40%
#'
#' @export

breaks.calc <- function(x) {
  breaks <- rev(c(0.02, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40))
  # if (any(is.na(sp.data))) warning("NA's removed")
  # if (any(sp.data == 0, na.rm = TRUE)) warning("Densities contain 0's")
  # if (any(sp.data < 0, na.rm = TRUE)) warning("Densities contain values < 0")

  x <- x[!is.na(x)]

  data.len <- length(x)
  data.max <- max(x)
  data.min <- min(x)

  sp.data.sort <- sort(x, decreasing = TRUE)
  data.breaks.mid <- sapply(breaks, function(i) {
    sp.data.sort[ceiling(i * data.len)]
  })
  data.breaks <- c(data.min, data.breaks.mid, data.max)

  return(data.breaks)
}



#' Calculate abundances for each of cols.data
#' Assumes that all cols.data have NAs at same place
#' Abundance depends on crs code of provided spdf
#' @export

model.abundance <- function(sdm, cols.data) {
  # Calculate areas of polygons with no NAs
  sdm.area.m2 <- st_area(sdm)
  validate(need(all(units(sdm.area.m2)[[1]] == c("m", "m")), "Units error"))
  sdm.area <- as.numeric(sdm.area.m2) / 1e+06

  # Calculate abundance for each data column
  abunds <- sapply(cols.data, function(j) {
    sum(as.data.frame(sdm)[j] * sdm.area, na.rm = TRUE)
  })

  return(abunds)
}
