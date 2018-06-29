# @examples
# evaluation_metrics(vals.save$models.ll[[2]], vals.save$eval.data, 1, "sight", "count")

#' Evaluation metrics
#'
#' Calculate AUC, TSS, and RMSE for given presence and absence points and model predictions
#'
#' @param x an object of class sf; SDM predictions
#' @param y an object of class sf; validation data
#' @param x.idx name or index of column in \code{x} with
#'   prediction (density) values
#' @param y.idx name or index of column in \code{y} with
#'   presence/absence data
#' @param y.idx.count name or index of column in \code{y} with
#'   count data. If not NULL, then RMSE is calculated
#'
#' @importFrom ROCR performance
#' @importFrom ROCR prediction
#' @importFrom sf st_crs
#' @importFrom sf st_geometry
#' @importFrom sf st_intersects
#' @importFrom sf st_set_geometry
#' @importFrom sf st_sf
#'
#' @return A three-element, numeric vector with AUC, TSS and RMSE values, respectively.
#'   If \code{y.idx.count} is NULL, then the RMSE value will be NA
#'
#'
#' @export
evaluation_metrics <- function(x, y, x.idx, y.idx, y.idx.count = NULL) {
  #-------------------------------------------------------------------
  if (!all(sapply(list(x, y), inherits, "sf"))) {
    stop("x and y must both be objects of class sf")
  }
  if (!identical(st_crs(x), st_crs(y))) {
    stop("x and y must have identical coordinate systems")
  }

  #-------------------------------------------------------------------
  if (!is.null(y.idx.count)) {
    x <- st_sf(
      cbind(st_set_geometry(x, NULL),
            abund = unlist(eSDM::model_abundance(x, x.idx, FALSE))),
      st_geometry(x), agr = "constant"
    )

    x.data2 <- st_set_geometry(x, NULL)[, "abund"]
    y.data2 <- st_set_geometry(y, NULL)[, y.idx.count]

  } else {
    x.data2 <- y.data2 <- NA
  }

  # TODO: will need to change this depending on what to do with points
  #   that lie on polygon borders
  yx.sgbp <- suppressMessages(st_intersects(y, x))

  x.data <- st_set_geometry(x, NULL)[, x.idx]
  y.data <- st_set_geometry(y, NULL)[, y.idx]

  xy.data.overlap <- data.frame(do.call(
    rbind,
    mapply(function(i, j) {
      if (length(j) == 0) {
        NULL

      } else {
        c(mean(x.data[i]), y.data[j], mean(x.data2[i]), y.data2[j])
      }
    }, yx.sgbp, seq_along(yx.sgbp), SIMPLIFY = FALSE)
  ))


  #------------------------------------------------------------
  # Calculate RMSE
  if (!is.null(y.idx.count)) {
    m3 <- sqrt(
      mean((xy.data.overlap[, 3] - xy.data.overlap[, 4]) ^ 2, na.rm = TRUE)
    )
  } else {
    m3 <- NA
  }

  #------------------------------------------------------------
  # Get ROCR prediction output
  pred.out <- prediction(xy.data.overlap[, 1], xy.data.overlap[, 2])

  #------------------------------------------------------------
  # AUC value
  m1 <- performance(pred.out, measure = "auc")@y.values[[1]]

  #------------------------------------------------------------
  # TSS value
  sens <- performance(pred.out, "sens")@y.values[[1]]
  spec <- performance(pred.out, "spec")@y.values[[1]]

  m2 <- max(sens + spec - 1)

  #------------------------------------------------------------
  c(m1, m2, m3)
}
