#' Calculates TSS for given presence and absence points and model predictions
#' Also calculates cutoff value for predictions that maximizes sens + spec
#'
#' @param x an object of class sf; validation data
#' @param y an object of class sf; SDM predictions
#' @param dens.idx name or index of column in \code{y} with density data
#' @param sight.idx name or index of column in \code{x} with sight data
#' @param prediction.out output of \code{eSDM::eval_prediction()}; if NA then
#'   \code{eSDM::eval_prediction(x, y, dens.idx, sight.idx)} is called within
#'   this function
#'
#' @return Returns TSS value (can return vector also with cutoff value)
#'
#' @export
eval_tss <- function(x, y, dens.idx, sight.idx, prediction.out = NA) {
  if (FALSE) {
    stop()
  }

  if (!inherits(prediction.out, "prediction")) {
    prediction.out <- eval_pred(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Calculate sensitivity, specificity, and then TSS
  sens <- slot(performance(prediction.out, "sens"), "y.values")[[1]]
  spec <- slot(performance(prediction.out, "spec"), "y.values")[[1]]

  max(sens + spec - 1)
}


#' Functions for calculating metric values
#'
#' @inheritParams eval_tss
#' @param plot.name the title of the ROCR plot; if \code{NA} then
#'   the ROCR plot is not printed
#'
#' @return  AUC value for given validation data and SDM predictions
#'
#' @export
eval_auc <- function(x, y, dens.idx, sight.idx, prediction.out = NA,
                     plot.name = NA) {
  if (FALSE) {
    stop()
  }

  if (!inherits(prediction.out, "prediction")) {
    prediction.out <- eSDM::eval_prediction(x, y, dens.idx, sight.idx)
  }

  if (!is.na(plot.name)) {
    plot(performance(prediction.out, measure = "tpr", x.measure = "fpr"),
         main = plot.name)
    graphics::box()
    abline(a = 0, b = 1)
  }

  slot(performance(prediction.out, measure = "auc"), "y.values")[[1]]
}


#' RMSE
#'
#' @param x an object of class sf; validation data
#' @param y an object of class sf; SDM predictions
#' @param abund.idx name or index of column in \code{y} with abundance data
#' @param count.idx thing 1
#' @param overlap.out thing 2
#' @param dens.idx thing 3
#'
#' @return RMSE for given presence numbers and model predictions
#'
#' @export
eval_rmse <- function(x, y, abund.idx, count.idx, overlap.out = NA,
                      dens.idx = NA) {
  if (FALSE) {
    stop()
  }

  if (!inherits(overlap.out, "data.frame")) {
    overlap.out <- eval_overlap(x, y, dens.idx)
  }

  # Calculate RMSE
  sqrt(mean((overlap.out[, abund.idx] - overlap.out[, count.idx]) ^ 2,
            na.rm = TRUE))
}
