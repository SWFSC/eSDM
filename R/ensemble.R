#' Rescale SDM predictions
#'
#' Rescale specified columns for each element in a list of SDM predictions
#'
#' @param x object of class \code{sf}
#' @param x.idx vector of column names or column indices;
#'   indicates which columns in \code{x} will be rescaled
#' @param y rescaling method; must be either "abundance" or "sumto1".
#'   See 'Details' section for descriptions of the rescaling methods
#' @param y.abund numeric value; ignored if \code{y} is not \code{"abundance"}
#'
#' @importFrom sf st_sf
#' @importFrom sf st_geometry
#' @importFrom sf st_set_geometry
#'
#' @details \code{ensemble_rescale} is intended to be used after overlaying predictions with \code{\link{overlay_sdm}} and
#'   before creating ensembles with \code{\link{ensemble_create}}.
#'   The provided rescaling methods are as follows:
#'   \describe{
#'   \item{'abundance' - Rescale the density values so that the predicted abundance is \code{y.abund}}{}
#'   \item{'sumto1' - Rescale the density values so their sum is 1}{}
#'   }
#'
#' @return The \code{sf} object \code{x} with the columns specified by \code{x.idx} rescaled
#'
#' @examples
#' ensemble_rescale(preds.1, c("Density", "Density2"), "abundance", 50)
#' ensemble_rescale(preds.1, c(1, 2), "sumto1")
#'
#' @export
ensemble_rescale <- function(x, x.idx, y, y.abund = NULL) {
  #----------------------------------------------------------------------------
  stopifnot(
    inherits(x, "sf"),
    length(x.idx) < ncol(x)
  )

  #--------------------------------------------------------
  x.df <- st_set_geometry(x, NULL)
  x.geom <- st_geometry(x)
  if (inherits(x.idx, "character")) {
    if (!all(x.idx %in% names(x.df))) {
      stop("If x.idx is a character vector, then all elements of x.idx must ",
           "be the name of a column of x (and not the geometry list-column)")
    }

  } else if (inherits(x.idx, c("integer", "numeric"))) {
    if (!(max(x.idx) < ncol(x))) {
      stop("If x.idx is a numeric vector, then all values of x must be ",
           "less than ncol(x)")
    }
    x.idx <- names(x)[x.idx]

  } else {
    stop("x.idx must be a vector of class character, integer, or numeric")
  }

  #--------------------------------------------------------
  if (!(y %in% c("abundance", "sumto1"))) {
    stop("y must be either 'abundance' or 'sumto1'")
  }

  #----------------------------------------------------------------------------
  x.df.idx <- x.df[, x.idx]

  #--------------------------------------------------------
  if (y == "abundance") {
    if (!(y.abund > 0 & inherits(y.abund, "numeric"))) {
      stop ("If y is 'abundance', y.abund must be a number greater than 0")
    }

    x.df.idx.rescaled <- data.frame(lapply(x.df.idx, function(i, j, k) {
      tmp.sf <- st_sf(pred = i, geometry = j, agr = "constant")
      i / (eSDM::model_abundance(tmp.sf, "pred") / k)
    }, j = x.geom, k = y.abund))

    x.df[, x.idx] <- x.df.idx.rescaled

  } else {
    #------------------------------------------------------
    x.df.idx.rescaled <- data.frame(lapply(x.df.idx, function(i) {
      i / sum(i, na.rm = TRUE)
    }))

    x.df[, x.idx] <- x.df.idx.rescaled
  }

  #----------------------------------------------------------------------------

  st_sf(x.df, geometry = x.geom, agr = "constant")
}


#' Create ensemble of SDM predictions
#'
#' Create a weighted or unweighted ensemble of SDM predictions
#'
#' @param x object of class \code{sf}
#' @param x.idx vector of column names or numerical indices;
#'   indicates which columns in \code{x} will be used to create the ensemble
#' @param y weights for the ensemble; either a numeric vector the same length as \code{x} or
#'   a data frame (or tibble) with the same number of rows as \code{x} and \code{ncol(y) == length(x.idx)}.
#'   If y is a numeric vector, its values (i.e. the weights) must sum to 1.
#'   The default value is \code{1 / length(x.idx)}, i.e. an unweighted ensemble
#'
#' @importFrom dplyr select
#' @importFrom sf st_geometry
#' @importFrom sf st_set_geometry
#' @importFrom sf st_sf
#' @importFrom stats weighted.mean
#'
#' @details \code{ensemble_create} is intended to be used after overlaying predictions with \code{\link{overlay_sdm}} and
#'   (if desired) rescaling the overlaid predictions with \code{\link{ensemble_rescale}}.
#'
#'   \code{ensemble_create} includes functionality for ensembling methods provided in \link{eSDM_GUI},
#'   although not for regional weighting', which currently must be done manually if not using the GUI.
#'   For instance, if \code{y.weights} is a data frame, then the 'Pixel-level spatial weights' ensembling method is performed.
#'
#' @return object of class \code{sf} with two columns: 'Pred.ens' and 'geometry';
#'   'Pred_ens' constists of the ensemble predictions and 'geometry' is the simple feature geometry list-column.
#'   The \code{sf} object attribute \code{agr} is set as 'constant' for 'Pred.ens'
#'
#' @examples
#' ensemble_create(preds.1, c("Density", "Density2"))
#' ensemble_create(preds.1, c(1, 2), c(0.2, 0.8))
#'
#' weights.df <- data.frame(runif(325), c(rep(NA, 100), runif(225)))
#' ensemble_create(preds.1, c("Density", "Density2"), weights.df)
#'
#' @export
ensemble_create <- function(x, x.idx, y = NULL) {
  #----------------------------------------------------------------------------
  stopifnot(
    inherits(x, "sf"),
    length(x.idx) < ncol(x)
  )

  #--------------------------------------------------------
  x.df <- st_set_geometry(x, NULL)
  if (inherits(x.idx, "character")) {
    if (!all(x.idx %in% names(x.df))) {
      stop("If x.idx is a character vector, then all elements of x.idx must ",
           "be the name of a column of x (and not the geometry list-column)")
    }

  } else if (inherits(x.idx, c("integer", "numeric"))) {
    if (!(max(x.idx) < ncol(x))) {
      stop("If x.idx is a numeric vector, then all values of x must be ",
           "less than ncol(x)")
    }
    x.idx <- names(x)[x.idx]

  } else {
    stop("x.idx must be a vector of class character, integer, or numeric")
  }

  #--------------------------------------------------------
  if (is.null(y)) {
    y <- rep(1 / length(x.idx), length(x.idx))

  } else if (inherits(y, c("data.frame", "tibble"))) {
    stopifnot(
      ncol(y) == length(x.idx),
      nrow(x) == nrow(y)
    )

  } else if (inherits(y, "numeric")) {
    if (length(x.idx) != length(y)) stop("x.idx and y must have the same length")
    if (!(sum(y) == 1)) stop("If y is a numeric vector, it must sum to 1")

  } else {
    stop("y must be one of NULL, a data frame (or tibble), or a numeric vector")
  }


  #----------------------------------------------------------------------------
  x.df.idx   <- x.df %>% select(!!x.idx)
  x.df.noidx <- x.df %>% select(!!-x.idx)

  if (inherits(y, "numeric")) {
    data.ens <- apply(
      x.df.idx, 1, function(i, j) weighted.mean(i, j, na.rm = TRUE), j = y
    )

  } else { #y is data frame
    warning("SMW todo")
    data.ens <- apply(x.df.idx * y, 1, mean, na.rm = TRUE)
  }

  stopifnot(is.numeric(data.ens))
  data.ens[is.nan(data.ens)] <- NA

  st_sf(
    data.frame(Pred_ens = data.ens, x.df.noidx),
    geometry = st_geometry(x), agr = "constant"
  )
}
