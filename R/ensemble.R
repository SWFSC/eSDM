#' Rescale SDM predictions
#'
#' Rescale specified columns for each element in a list of SDM predictions
#'
#' @param x list of objects of class \code{sf};
#'   all objects must have the same number of rows
#' @param x.pred.idx character or numeric vector of names or column indices of predicted density column
#'   to be rescaled for each element of \code{x}; must be the same length as \code{x}
#' @param y rescaling method; must be one of: "abundance", "normalization", "standardization", or "sumto1".
#'   See 'Details' section for descriptions of the rescaling methods
#' @param y.abund numeric value; ignored if \code{y != "abundance"}
#'
#' @importFrom purrr set_names
#' @importFrom sf st_sf
#' @importFrom sf st_geometry
#' @importFrom sf st_set_geometry
#'
#' @details \code{ensemble_rescale} is intended to be used between overlaying predictions with \code{\link{overlay_sdm}} and
#'   creating ensembles with \code{\link{ensemble_create}},
#'   which is why all elments of \code{x} must have the same number of rows.
#'   The rescaling methods are as follows:
#'   \describe{
#'   \item{'abundance' - Rescale the density values so that the predicted abundance is \code{y.abund}}{}
#'   \item{'normalization' - Rescale the density values to a range of [0, 1]}{}
#'   \item{'standardization' - Rescale the density values to have a mean of 0 and standard deviation of 1}{}
#'   \item{'sumto1' - Rescale the density values so their sum is 1}{}
#'   }
#'
#' @return The list \code{x}, but with the specified columns rescaled
#'
#' @examples
#' x <- list(
#'   preds.1, overlay_sdm(sf::st_geometry(preds.1), preds.2, "Density", 50)
#' )
#' ensemble_rescale(x, c("Density", "Density.overlaid"), "abundance", 50)
#' ensemble_rescale(x, c(1, 1), "normalization")
#'
#' @export
ensemble_rescale <- function(x, x.pred.idx, y, y.abund = NULL) {
  #----------------------------------------------------------------------------
  stopifnot(all(sapply(x, inherits, "sf")))
  if (!all(nrow(x[[1]]) == sapply(x, nrow))) {
    stop("All elements of x must have the same number of rows, ",
         "as should be the case if they are overlaid")
  }
  if (length(x.pred.idx) != length(x)) {
    stop("x and x.pred.idx must have the same number of elements")
  }
  if (!(y %in% c("abundance", "normalization", "standardization", "sumto1"))) {
    stop("y must be one of: 'abundance', 'normalization', ",
         "'standardization', or 'sumto1'")
  }

  #----------------------------------------------------------------------------
  #--------------------------------------------------------
  if (y == "abundance") {
    if (!(y.abund > 0)) {
      stop ("y.abund must be a number greater than 0 if y is 'abundance'")
    }

    data.rescaled <- data.frame(
      mapply(function(i, j) {
        st_set_geometry(i, NULL)[, j] / (eSDM::model_abundance(i, j) / y.abund)
      }, x, x.pred.idx, SIMPLIFY = FALSE)
    ) %>% set_names(paste0("x.", 1:length(x)))

  } else {
    #------------------------------------------------------
    data.extracted <- data.frame(
      mapply(function(i, j) {
        st_set_geometry(i, NULL)[, j]
      }, x, x.pred.idx, SIMPLIFY = FALSE)
    ) %>% set_names(paste0("x.", 1:length(x)))

    if (y == "normalization") {
      data.rescaled <- as.data.frame(apply(data.extracted, 2, function(i) {
        if (diff(range(i, na.rm = TRUE)) == 0) {
          stop("At least one of the specified data columns has a range of 0; ",
               "you cannot normalize a vector of numbers with a range of 0")
        } else {
          esdm_normalize(i)
        }
      }))

    } else if (y == "standardization") {
      data.rescaled <- as.data.frame( apply(data.extracted, 2, function(i) {
        if (diff(range(i, na.rm = TRUE)) == 0) {
          stop("At least one of the specified data columns has a range of 0; ",
               "you cannot standardize a vector of numbers with a range of 0")
        } else {
          base::scale(i)
        }
      }))

    } else { #y == "sumto1"
      data.rescaled <- as.data.frame(
        apply(data.extracted, 2, function(i) {i / sum(i, na.rm = TRUE)})
      )
    }
  }

  #----------------------------------------------------------------------------
  lapply(1:length(x), function(i) {
    new.df <- st_set_geometry(x[[i]], NULL)
    new.df[, x.pred.idx[i]] <- data.rescaled[, i]
    st_sf(new.df, geometry = st_geometry(x[[1]]), agr = "constant")
  })
}


#' Create ensemble of SDM predictions
#'
#' Create weighted or unweighted ensemble of SDM predictions
#'
#' @param x list of objects of class \code{sf}, all of which must have the same geometry
#' @param x.pred.idx vector of names or column indices giving the predictions column for each element of \code{x};
#'   must be the same length as \code{x}
#' @param y ensembling method; one of: "unweighted", "weighted"
#' @param y.weights either a numeric vector the same length as \code{x} or
#'   a data frame with \code{ncol(y.weights) == length(x)} and the same number of rows as each element of \code{x};
#'   ignored if \code{y == "unweighted"}
#'
#' @importFrom sf st_geometry
#' @importFrom sf st_set_geometry
#' @importFrom sf st_sf
#' @importFrom stats weighted.mean
#'
#' @details \code{ensemble_create} is intended to be used after overlaying predictions with \code{\link{overlay_sdm}} and
#'   (if desired) rescaling the overlaid predictions with \code{\link{ensemble_rescale}},
#'   which is why all elments of \code{x} must have the same geometry.
#'
#'   \code{ensemble_create} includes functionality for ensembling methods provided in \link{esdm_gui},
#'   although not for regional weighting', which currently must be done manually if not using the GUI.
#'   For instance, if \code{y.weights} is a data frame, then the 'Pixel-level spatial weights' ensembling method is performed.
#'
#' @return object of class \code{sf} with two columns: 'Pred.ens' and 'geometry';
#'   'Pred.ens' constists of the ensemble predictions and 'geometry' is the simple feature geometry list-column.
#'   The \code{sf} object attribute \code{agr} is set as 'constant' for 'Pred.ens'
#'
#' @examples
#' x <- list(
#'   preds.1, preds.1, overlay_sdm(sf::st_geometry(preds.1), preds.2, "Density", 50)
#' )
#' ensemble_create(x, c("Density", "Density2", "Density.overlaid"), "unweighted")
#' ensemble_create(x, c("Density", "Density2", "Density.overlaid"), "weighted", c(0.5, 1, 0.8))
#'
#' weights.df <- data.frame(runif(325), runif(325), c(rep(NA, 100), runif(225)))
#' ensemble_create(x, c("Density", "Density2", "Density.overlaid"), "weighted", weights.df)
#'
#' @export
ensemble_create <- function(x, x.pred.idx, y, y.weights = NULL) {
  #--------------------------------------------------------
  stopifnot(all(sapply(x, inherits, "sf")))
  if (length(x.pred.idx) != length(x)) {
    stop("x and x.pred.idx must have the same number of elements")
  }
  if (!(y %in% c("unweighted", "weighted"))) {
    stop("y must be one of: 'unweighted' or 'weighted'")
  }
  x1.geom <- st_geometry(x[[1]])
  if (!all(sapply(x[-1], function(i) identical(x1.geom, st_geometry(i))))) {
    stop("All elements of x must have the same geometry")
  }

  if (y == "weighted") {
    if (inherits(y.weights, "numeric")) {
      if (length(y.weights) != length(x)) {
        stop("If y.weights is a numeric vector, then x and y.weights ",
             "must have the same number of elements")
      }

    } else if (inherits(y.weights, "data.frame")) {
      if (ncol(y.weights) != length(x) | nrow(y.weights) != length(x1.geom)) {
        stop("If y.weights is a data frame, then it must have the ",
             "same number of columns as x has elements ",
             "and the same number of rows as each element of x")
      }

    } else {
      stop("y.weights must be either a numeric vector or a data frame")
    }
  }


  #--------------------------------------------------------
  data.toens <- mapply(function(i, j) {
    st_set_geometry(i, NULL)[, j]
  }, x, x.pred.idx, SIMPLIFY = FALSE)
  data.toens <- data.frame(do.call(cbind, data.toens))

  stopifnot(all(apply(data.toens, 2, inherits, "numeric")))


  #--------------------------------------------------------
  if (y == "unweighted") {
    data.ens <- apply(data.toens, 1, mean, na.rm = TRUE)

  } else { #y == "weighted"
    if (inherits(y.weights, "numeric")) {
      data.ens <- apply(
        data.toens, 1, function(p) weighted.mean(p, y.weights, na.rm = TRUE)
      )

    } else { #inherits(y.weights, "data.frame")
      data.toens <- data.toens * y.weights
      data.ens <- apply(data.toens, 1, mean, na.rm = TRUE)
    }
  }

  stopifnot(is.numeric(data.ens))
  data.ens[is.nan(data.ens)] <- NA

  st_sf(Pred.ens = data.ens, geometry = x1.geom, agr = "constant")
}
