#' Helper functions for calculating model evaluation metrics
#'
#' Return df with overlap of presence/absence data and model
#' Returned df contains data columns for density, abundance, ...
#' ...sightings (p/a)) flag, and number of animals observed
#' Helper function 1 for AUC, TSS, and RMSE calcs
#'
#' @param x an object of class sf; validation data
#' @param y an object of class sf; SDM predictions
#' @param dens.idx name or index of column in \code{x} with density data;
#'   if \code{NA}, then a column with predicted abundance
#'   is not included in the ouput
#'
#' @return data.frame
#'
#' @export

eval_overlap <- function(x, y, dens.idx = NA) {
  if (!all(sapply(list(x, y), inherits, "sf"))) {
    stop("x and y must both be objects of class sf")
  }
  if (!identical(st_crs(x), st_crs(y))) {
    stop("x and y must have identical coordinate systems")
  }

  if (!is.na(dens.idx)) {
    y <- st_sf(
      cbind(st_set_geometry(y, NULL),
            abund = unlist(model_abundance(y, dens.idx, FALSE))),
      st_geometry(y), agr = "constant"
    )
  }

  # TODO: will need to change this depending on what to do with points
  #   that lie on polygon borders
  xy.sgbp <- suppressMessages(st_intersects(x, y))
  xy.sgbp.mult <- which(sapply(xy.sgbp, length) > 1)

  x.df <- st_set_geometry(x, NULL)
  y.df <- st_set_geometry(y, NULL)
  d <- do.call(
    rbind,
    mapply(function(i, j) {
      i.len <- length(i)
      if (i.len == 0) {
        NULL

      } else if (i.len == 1) {
        cbind(x.df[j, ], y.df[i, ])

      } else {
        unlist(
          c(x.df[j, ], apply(y.df[i, ], 2, mean))
        )
      }
    }, xy.sgbp, seq_along(xy.sgbp), SIMPLIFY = FALSE)
  )

  d
  # list(d, xy.sgbp.mult)
}


#' Returns ROCR::prediction() output of density and sightings columns
#' Helper function 2 for AUC and TSS calcs
#'
#' @param x an object of class sf; validation data
#' @param y an object of class sf; SDM predictions
#' @param dens.idx name or index of column in \code{x} with density data
#' @param sight.idx name or index of column in \code{x} with sight data
#' @param overlap.out output of \code{eSDM::eval_overlap)()}; if NA then
#'   \code{eSDM::eval_overlap(x, y, dens.idx)} is called within this function
#'
#' @return ROCR::prediction() output of density and sightings columns
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
  if (!inherits(overlap.out, "data.frame")) {
    overlap.out <- eval_overlap(x, y, dens.idx)
  }

  ROCR::prediction(overlap.out[, dens.idx], overlap.out[, sight.idx])
}
