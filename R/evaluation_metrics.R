#' Helper functions for calculating model evaluation metrics
#'
#' Return df with overlap of presence/absence data and model
#' Returned df contains data columns for density, abundance, ...
#' ...sightings (p/a)) flag, and number of animals observed
#' Helper function 1 for AUC, TSS, and RMSE calcs
#'
#' @export

eval_overlap <- function(pres.pts, abs.pts, model.data, model.dens.idx) {
  stopifnot(
    all(sapply(list(pres.pts, abs.pts, model.data), inherits, "sf"))
  )

  if (!(identical(st_crs(pres.pts), st_crs(abs.pts)) &
        identical(st_crs(pres.pts), st_crs(model.data)))) {
    stop("Presence points, absence points, and model predictions must ",
         "have identical coordinate systems")
  }

  browser()
  # Overlap of validation data and model
  data.dens <- model.data@data[ ,model.dens.idx]
  model.data@data <- data.frame(dens = data.dens)
  model.data$abund <- model.data$dens * (area(model.data) / 1e+06)

  pres.model.over <- over(pres.pts, model.data)
  abs.model.over <- over(abs.pts, model.data)
  pres.model.over$sight <- rep(1, nrow(pres.model.over))
  abs.model.over$sight <- rep(0, nrow(abs.model.over))
  pres.model.over$num <- pres.pts$pa.num
  abs.model.over$num <- abs.pts$pa.num

  model.pa <- rbind(pres.model.over, abs.model.over)
  model.pa.nona <- model.pa[!is.na(model.pa$dens), ]

  return(model.pa.nona)
}


#################################################
#' Returns ROCR::prediction() output of density and sightings columns
#' Helper function 2 for AUC and TSS calcs
#'
#' @export

eval_prediction <- function(pres.pts, abs.pts, model.data, model.dens.idx,
                            overlap.out = NA) {
  # Check the point and model have same crs
  validate(
    need(identicalCRS(pres.pts, abs.pts),
         paste("Error: Presence and absence points do not",
               "have identical projections")),
    need(identicalCRS(pres.pts, model.data),
         paste("Error: Presence points and model predictions do not",
               "have identical projections"))
  )

  # Overlap of presence/absence data and model
  model.pa.df <- overlap.out
  if (all(is.na(model.pa.df))) {
    model.pa.df <- eval_over(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Prediction
  pred <- prediction(model.pa.df$dens, model.pa.df$sight)

  return(pred)
}


###############################################################################
#' Functions for calculating metric values
#'
#' @return  AUC for given p/a points and model predictions
#'
#' @export

eval_auc <- function(pres.pts, abs.pts, model.data, model.dens.idx,
                     plot.name = "", prediction.out = NA) {
  # Get overlap of pres/abs data and model and create prediction object
  pred <- prediction.out
  if (suppressWarnings(is.na(pred))) {
    pred <- eval_pred(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Calculate metric
  perf.auc <- performance(pred, measure = "auc")

  # perf.roc <- performance(pred, measure = "tpr", x.measure = "fpr")
  # plot(perf.roc, main = plot.name)
  # abline(a=0, b= 1)

  return(perf.auc@y.values[[1]])
}


#################################################
#' Calculates TSS for given presence and absence points and model predictions
#' Also calculates cutoff value for predictions that maximizes sens + spec
#'
#' @return Returns TSS value (can return vector also with cutoff value)
#'
#' @export

eval_tss <- function(pres.pts, abs.pts, model.data, model.dens.idx,
                     prediction.out = NA) {
  # Get overlap of pres/abs data and model and create prediction object
  pred <- prediction.out
  if (suppressWarnings(is.na(pred))) {
    pred <- helper.pred(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Calculate metrics and then TSS
  perf.sens <- performance(pred, measure = "sens")
  sens <- perf.sens@y.values[[1]]

  perf.spec <- performance(pred, measure = "spec")
  spec <- perf.spec@y.values[[1]]

  tss <- sens + spec - 1
  idx <- which.max(tss)
  tss.max <- tss[idx]

  # cutoff.max <- perf.sens@x.values[[1]][idx]
  # cutoff.max <- cutoff.max.adj * data.dens.max

  return (tss.max) #c(tss.max, cutoff.max)
}


#################################################
#' RMSE
#'
#' @return RMSE for given presence numbers and model predictions
#'
#' @export

eval_rmse <- function(pres.pts, abs.pts, model.data, model.dens.idx,
                      prediction.out = NA) {
  # Get overlap of pres/abs data and model
  model.pa.df <- prediction.out
  if (all(is.na(model.pa.df))) {
    model.pa.df <- helper.over(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Calculate RMSE
  rmse.val <- sqrt(mean((model.pa.df$abund - model.pa.df$num)^2))

  return(rmse.val)
}

###############################################################################
