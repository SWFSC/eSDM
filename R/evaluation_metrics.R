#' Helper functions for calculating model evaluation metrics
#'
#' Return df with overlap of presence/absence data and model
#' Returned df contains data columns for density, abundance, ...
#' ...sightings (p/a)) flag, and number of animals observed
#' Helper function 1 for AUC, TSS, and RMSE calcs
#'
#' @export

eval_overlap <- function(x, y, dens.idx = NA) {
  stopifnot(
    all(sapply(list(x, y), inherits, "sf"))
  )

  if (!identical(st_crs(x), st_crs(y))) {
    stop("Validation data points and model predictions must ",
         "have identical coordinate systems")
  }

  if (!is.na(dens.idx)) {
    y <- st_sf(cbind(st_set_geometry(y, NULL),
                     abund = unlist(model_abundance(y, dens.idx, FALSE))),
               st_geometry(y), agr = "constant")
  }

  # TODO: will need to change this depending on what to do with points
  #   that lie on polygon borders
  suppressMessages(st_intersection(x, y)) %>%
    st_set_geometry(NULL)
}


#' Returns ROCR::prediction() output of density and sightings columns
#' Helper function 2 for AUC and TSS calcs
#'
#' @export

eval_prediction <- function(x, y, dens.idx, sight.idx, overlap.out = NA) {
  if (!inherits(overlap.out, "data.frame")) {
    # overlap.out is NA
    if (!(inherits(x, "data.frame") & inherits(y, "data.frame"))) {
      stop("If overlap.out is not a data.frame object then ",
           "x and y must both be data.frame objects")

    } else if (!identical(st_crs(x), st_crs(y))) {
      stop("x any y must have the same crs")

    } else if(!(dens.idx %in% names(y) & sight.idx %in% names(x))) {
      stop("dens.idx and sight.idx must either be a name of ",
           "one of the columns of y and x, respectively. ",
           "Since the column indices might change during overlap")
    }

  } else {
    # overlap.out is not NA
    if (!((is.numeric(dens.idx) | dens.idx %in% names(overlap.out)) &
          is.numeric(sight.idx) | sight.idx %in% names(overlap.out))) {
      stop("dens.idx and sight.idx must be a number or a name of ",
           "one of the columns of overlap.out")    }
    if (sum(sapply(list(x, y, overlap.out), inherits, "data.frame")) > 1) {
      warning("If overlap.out is not NA then x and y will be ignored")
    }
  }


  # If overlap.out was not povided, calculate now
  if (!inherits(overlap.out, "data.frame")) overlap.out <- eval_overlap(x, y)

  ROCR::prediction(overlap.out[, dens.idx], overlap.out[, sight.idx])
}


#------------------------------------------------------------------------------
#' Functions for calculating metric values
#'
#' @return  AUC for given p/a points and model predictions
#'
#' @export

eval_auc <- function(x, y, dens.idx, sight.idx, prediction.out = NA,
                     plot.name = NA) {
  if (FALSE) {
    stop()
  }

  if (!inherits(prediction.out, "prediction")) {
    prediction.out <- eval_pred(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Calculate metric
  if (!is.na(plot.name)) {
    plot(performance(prediction.out, measure = "tpr", x.measure = "fpr"),
         main = plot.name)
    graphics::box()
    abline(a = 0, b = 1)
  }

  slot(performance(prediction.out, measure = "auc"), "y.values")[[1]]
}


#' Calculates TSS for given presence and absence points and model predictions
#' Also calculates cutoff value for predictions that maximizes sens + spec
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


#' RMSE
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
