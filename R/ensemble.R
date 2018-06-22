#' Rescale SDM predictions
#'
#' Rescale specified columns in a list of SDM prediction
#'
#' @param x is a list of sf objects;
#' all sf objects must have the same number of rows
#' @param x.pred.idx names or column indices of predicted density column in
#'   each element of x; must be the same length as x
#' @param y rescaling method;
#' one of: "abundance", "normalization", "standardization", or "sumto1"
#' @param y.abund value to which to resclae abundances of x;
#'   ignored if y is not "abundance"
#'
#' @export
ensemble_rescale <- function(x, x.pred.idx, y, y.abund = NULL) {
  #--------------------------------------------------------
  stopifnot(
    all(sapply(x, nrow) == nrow(x[[1]])),
    length(x.pred.idx) == length(x),
    y %in% c("abundance", "normalization", "standardization", "sumto1")
  )


  #--------------------------------------------------------
  if (y == "abundance") {
    if (!(y.abund > 0)) {
      stop ("y.abund must be a number greater than 0 if y is 'abundance'")
    }

    data.rescaled <- data.frame(
      mapply(function(i, j) {
        st_set_geometry(i, NULL)[, j] / (eSDM::model_abundance(i, j) / y.abund)
      }, x, x.pred.idx, SIMPLIFY = FALSE)
    ) %>% purrr::set_names(paste0("x.", 1:length(x)))

  } else {
    data.extracted <- data.frame(
      mapply(function(i, j) {
        st_set_geometry(i, NULL)[, j]
      }, x, x.pred.idx, SIMPLIFY = FALSE)
    ) %>% purrr::set_names(paste0("x.", 1:length(x)))

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
    st_sf(
      new.df,
      geometry = st_geometry(x[[1]]), agr = "constant"
    )
  })
}
