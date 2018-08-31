#' Evaluation metrics
#'
#' Calculate AUC, TSS, and RMSE for given density predictions and validation data
#'
#' @param x object of class sf; SDM predictions
#' @param y object of class sf; validation data
#' @param x.idx name or index of column in \code{x} with prediction values
#' @param y.idx name or index of column in \code{y} with validation data
#' @param count.flag logical; if \code{TRUE} the data in column \code{y.idx} is count data,
#'   and if \code{FALSE} the data in column \code{y.idx} is presence/absence
#'
#' @importFrom ROCR performance
#' @importFrom ROCR prediction
#' @importFrom sf st_crs
#' @importFrom sf st_intersects
#' @importFrom sf st_set_geometry
#'
#' @details If \code{count.flag == TRUE}, then \code{eSDM::model_abundance(x, x.idx, FALSE)} will be run
#'   to calculate predicted abundance and thus calculate RMSE.
#'   Note that this assumes the data in column \code{x.idx} of \code{x} are density values.
#'   If \code{count.flag == FALSE}, then all of the values in column \code{x.idx} of \code{x} must be \code{0} or \code{1}.
#'   All rows of \code{x} with a value of \code{NA} in column \code{x.idx} are removed within the function
#'
#' @return A numeric vector with AUC, TSS and RMSE values, respectively.
#'   If \code{count.flag == FALSE}, the RMSE value will be \code{NA}
#'
#' @examples
#' evaluation_metrics(preds.1, validation.data, 1, "sight")
#'
#' evaluation_metrics(preds.1, validation.data, 1, "count", TRUE)
#'
#' @export
evaluation_metrics <- function(x, y, x.idx, y.idx, count.flag = FALSE) {
  #------------------------------------------------------------------
  if (!all(sapply(list(x, y), inherits, "sf"))) {
    stop("x and y must both be objects of class sf")
  }
  if (!identical(st_crs(x), st_crs(y))) {
    stop("x and y must have identical coordinate systems")
  }
  stopifnot(inherits(count.flag, "logical"))

  x.dens <- st_set_geometry(x, NULL)[, x.idx]
  x.dens <- x.dens[!is.na(x.dens)]
  y.temp <- st_set_geometry(y, NULL)[, y.idx]

  if (!inherits(x.dens,  c("numeric", "integer"))) {
    stop("The data in column x.idx of object x must be numeric")
  }
  if (!inherits(y.temp, c("numeric", "integer"))) {
    stop("The data in column y.idx of object y must be numeric")
  }


  #------------------------------------------------------------------
  x <- x[!is.na(st_set_geometry(x, NULL)[, 1]), ]
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
    x.abund <- unname(unlist(eSDM::model_abundance(x, x.idx, FALSE)))
    y.sight <- ifelse(y.temp >= 1, 1, 0)
    y.count <- y.temp

  } else {
    if (!all(y.temp %in% c(0, 1))) {
      stop("The data in column y.idx of object y must all be numbers 0 or 1")
    }

    x.abund <- as.numeric(NA)
    y.sight <- y.temp
    y.count <- as.numeric(NA)
  }
  rm(y.temp)

  stopifnot(
    inherits(x.abund, c("numeric", "integer")),
    inherits(y.sight, c("numeric", "integer")),
    inherits(y.count, c("numeric", "integer"))
  )
  stopifnot(length(unique(y.sight)) == 2)


  #------------------------------------------------------------------
  xy.data.overlap <- data.frame(do.call(
    rbind,
    mapply(function(i, j) {
      if (length(j) == 0) {
        NULL
      } else {
        c(mean(x.dens[i]), y.sight[j], mean(x.abund[i]), y.count[j])
      }
    }, yx.sgbp, seq_along(yx.sgbp), SIMPLIFY = FALSE)
  ))


  #------------------------------------------------------------------
  # RMSE
  if (count.flag) {
    m3 <- esdm_rmse(xy.data.overlap[, 3], xy.data.overlap[, 4])
  } else {
    m3 <- NA
  }

  #----------------------------------------------
  pred.out <- prediction(xy.data.overlap[, 1], xy.data.overlap[, 2])

  #----------------------------------------------
  # AUC
  m1 <- performance(pred.out, measure = "auc")@y.values[[1]]

  #----------------------------------------------
  # TSS
  sens <- performance(pred.out, "sens")@y.values[[1]]
  spec <- performance(pred.out, "spec")@y.values[[1]]
  m2 <- max(sens + spec - 1)


  #------------------------------------------------------------------
  c(m1, m2, m3)
}
