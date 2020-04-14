#' Calculate SDM evaluation metrics
#'
#' Calculate AUC, TSS, and RMSE for given density predictions and validation data
#'
#' @param x object of class sf; SDM predictions
#' @param x.idx name or index of column in \code{x} with prediction values
#' @param y object of class sf; validation data
#' @param y.idx name or index of column in \code{y} with validation data.
#'   This validation data column must have at least two unique values, e.g. 0 and 1
#' @param count.flag logical; \code{TRUE} indicates that the data in column \code{y.idx} is count data,
#'   while \code{FALSE} indicates that the data is presence/absence.
#'   See details for differences in data processing based on this flag.
#'
#' @details If \code{count.flag == TRUE}, then \code{eSDM::model_abundance(x, x.idx, FALSE)} will be run
#'   to calculate predicted abundance and thus calculate RMSE.
#'   Note that this assumes the data in column \code{x.idx} of \code{x} are density values.
#'
#'   If \code{count.flag == FALSE}, then all of the values in column \code{y.idx} of \code{y} must be \code{0} or \code{1}.
#'
#'   All rows of \code{x} with a value of \code{NA} in column \code{x.idx} and
#'   all rows of \code{y} with a value of \code{NA} in column \code{y.idx} are removed before calculating metrics
#'
#' @return A numeric vector with AUC, TSS and RMSE values, respectively.
#'   If \code{count.flag == FALSE}, the RMSE value will be \code{NA}
#'
#' @examples
#' evaluation_metrics(preds.1, 2, validation.data, "sight")
#'
#' evaluation_metrics(preds.1, "Density2", validation.data, "count", TRUE)
#'
#' @export
evaluation_metrics <- function(x, x.idx, y, y.idx, count.flag = FALSE) {
  #------------------------------------------------------------------
  # Input checks and some processing
  if (!all(vapply(list(x, y), inherits, TRUE, "sf"))) {
    stop("x and y must both be objects of class sf")
  }
  if (!identical(st_crs(x), st_crs(y))) {
    stop("x and y must have identical coordinate systems")
  }
  stopifnot(
    length(x.idx) == 1,
    length(y.idx) == 1,
    inherits(count.flag, "logical")
  )

  x.dens <- st_set_geometry(x, NULL)[[x.idx]]
  x.dens.nona <- !is.na(x.dens)
  x.dens <- x.dens[x.dens.nona]

  y.data <- st_set_geometry(y, NULL)[[y.idx]]
  y.data.nona <- !is.na(y.data)
  y.data <- y.data[y.data.nona]

  if (!is.numeric(x.dens)) {
    stop("The data in column x.idx of object x must all be numbers")
  }
  if (!is.numeric(y.data)) {
    stop("The data in column y.idx of object y must all be numbers")
  }
  if (length(unique(y.data)) < 2) {
    stop("The data in column y.idx of object y must have at least ",
         "two unqiue values, e.g. 0 and 1. ",
         "Calculating metrics using presence-only or absence-only data ",
         "is not currently supported by this function")
  }

  #------------------------------------------------------------------
  # Remove NAs, then get intersection of predictions and validation data
  x <- x[x.dens.nona, ]
  y <- y[y.data.nona, ]
  yx.sgbp <- suppressMessages(st_intersects(y, x))

  temp <- sapply(yx.sgbp, length)
  temp0 <- sum(temp == 0)
  temp2 <- sum(temp > 1)
  if (temp0 > 0) {
    base::message(
      "There were ", temp0, " validation points ",
      "that did not overlap with a non-NA prediction polygon"
    )
  }
  if (temp2 > 0) {
    base::message(
      "There were ", temp2, " validation points ",
      "that were on the boundary of two or more non-NA prediction polygons"
    )
  }
  rm(temp, temp0, temp2)


  #------------------------------------------------------------------
  # Data kept as separate vectors because in mapply() accessing several vector
  #   objects is faster than accessing one data.frame
  if (count.flag) {
    y.sight <- ifelse(y.data >= 1, 1, 0)
    x.abund <- unname(unlist(eSDM::model_abundance(x, x.idx, sum.abund = FALSE)))
    y.count <- y.data

  } else {
    if (!all(y.data %in% c(0, 1))) {
      stop("The data in column y.idx of object y must all be numbers 0 or 1")
    }

    y.sight <- y.data
    x.abund <- as.numeric(NA)
    y.count <- as.numeric(NA)
  }

  stopifnot(
    is.numeric(y.sight), is.numeric(x.abund), is.numeric(y.count)
  )


  #------------------------------------------------------------------
  # Make data frame with corresponding density and validation data values
  xy.data.overlap.list <- mapply(function(i, j) {
    if (length(j) == 0) {
      NULL
    } else {
      c(mean(x.dens[i]), y.sight[j], mean(x.abund[i]), y.count[j])
    }
  }, yx.sgbp, seq_along(yx.sgbp), SIMPLIFY = FALSE)

  xy.data.overlap <- data.frame(do.call(rbind, xy.data.overlap.list)) %>%
    set_names(c("dens", "sight", "abund", "count")) %>%
    filter(!is.na(.data$dens), !is.na(.data$sight))


  #------------------------------------------------------------------
  # AUC and TSS
  pred.out <- prediction(xy.data.overlap[[1]], xy.data.overlap[[2]])

  m1 <- slot(performance(pred.out, measure = "auc"), "y.values")[[1]]

  sens <- slot(performance(pred.out, "sens"), "y.values")[[1]]
  spec <- slot(performance(pred.out, "spec"), "y.values")[[1]]
  m2 <- max(sens + spec - 1)

  # RMSE
  m3 <- ifelse(
    count.flag,
    esdm_rmse(xy.data.overlap[[3]], xy.data.overlap[[4]], na.rm = TRUE),
    NA
  )

  #------------------------------------------------------------------
  c(m1, m2, m3)
}
