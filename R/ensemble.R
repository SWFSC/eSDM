#' Rescale SDM predictions
#'
#' Rescale specified columns in a list of SDM prediction
#'
#' @param x is a list of objects of \code{sf};
#'   all objects must have the same number of rows
#' @param x.pred.idx vector of names or column indices of predicted density column for each element of \code{x};
#'   must be the same length as \code{x}
#' @param y rescaling method; must be one of: "abundance", "normalization", "standardization", or "sumto1".
#'   See 'Details' for descriptions of the rescaling methods
#' @param y.abund numeric value; ignored if \code{y} is not "abundance"
#'
#' @importFrom purrr set_names
#' @importFrom sf st_sf
#' @importFrom sf st_geometry
#' @importFrom sf st_set_geometry
#'
#' @details This function is intended to be used between overlaying predictions with \code{\link{overlay_sdm}} and
#'   creating ensembles with \code{\link{ensemble_create}},
#'   which is why all elelments of \code{x} must have the same number of rows.
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
#'   preds.1, overlay_sdm(sf::st_geometry(preds.1), preds.2, 50, "Density")
#' )
#' ensemble_rescale(x, c("Density", "Density.overlaid"), "abundance", 50)
#' ensemble_rescale(x, c(1, 1), "normalization")
#'
#' @export
ensemble_rescale <- function(x, x.pred.idx, y, y.abund = NULL) {
  #--------------------------------------------------------
  if (!all(nrow(x[[1]]) == sapply(x, nrow))) {
    stop("All elements of x must have the same number of rows, ",
         "as should be the case if they are overlaid")
  }
  if (!length(x.pred.idx) == length(x)) {
    stop("x and x.pred.idx must have the same number of elements")
  }
  if (!(y %in% c("abundance", "normalization", "standardization", "sumto1"))) {
    stop("y must be one of: 'abundance', 'normalization', 'standardization', ",
         "or 'sumto1'")
  }

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
    data.extracted <- data.frame(
      mapply(function(i, j) {
        st_set_geometry(i, NULL)[, j]
      }, x, x.pred.idx, SIMPLIFY = FALSE)
    ) %>% set_names(paste0("x.", 1:length(x)))

    if (y == "normalization") {
      data.rescaled <- as.data.frame(apply(data.extracted, 2, esdm_normalize))

    } else if (y == "standardization") {
      data.rescaled <- (apply(data.extracted, 2, base::scale))

    } else { #"sumto1"
      data.rescaled <- as.data.frame(
        apply(data.extracted, 2, function(i) {i / sum(i, na.rm = TRUE)})
      )
    }
  }

  #--------------------------------------------------------
  lapply(1:length(x), function(i) {
    new.df <- st_set_geometry(x[[i]], NULL)
    new.df[, x.pred.idx[i]] <- data.rescaled[, i]
    st_sf(new.df, geometry = st_geometry(x[[1]]), agr = "constant")
  })
}


#' Create ensemble of SDM predictions
#'
#' Description
#'
#' @param x is a list of sf objects;
#' all sf objects must have the same geometry
#' @param x.pred.idx vector of names or column indices of predicted density column in
#'   each element of x; must be the same length as x
#' @param y rescaling method;
#' one of: "abundance", "normalization", "standardization", or "sumto1"
#' @param y.abund value to which to rescale abundances of x;
#'   ignored if y is not "abundance"
#'
#' @export
ensemble_create <- function(x, x.pred.idx, y, y.abund = NULL) {


  if (!all(sapply(x, function(i) identical(st_geometry(x[[1]], st_geometry(i)))))) {
    stop("All objects in x must have the same geometry")
  }

  x
}
