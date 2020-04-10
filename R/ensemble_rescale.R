#' Rescale SDM predictions
#'
#' Rescale SDM predictions and (if applicable) associated uncertainties
#'
#' @param x object of class \code{sf}
#' @param x.idx vector of column names or column indices;
#'   indicates columns in \code{x} with prediction values that will be rescaled
#' @param y rescaling method; must be either "abundance" or "sumto1".
#'   See 'Details' section for descriptions of the rescaling methods
#' @param y.abund numeric value; ignored if \code{y} is not \code{"abundance"}
#' @param x.var.idx vector of column names or column indices;
#'   indicates columns in \code{x} with variance values that will be rescaled.
#'   If \code{x.var.idx} is specified, it must be the same length as \code{x.idx}.
#'   Use \code{x.var.idx = NULL} (the default) if none of the predictions have associated uncertainty values;
#'   see the 'Details' section for more information
#'
#' @details \code{ensemble_rescale} is intended to be used after overlaying predictions with
#'   \code{\link{overlay_sdm}} and before creating ensembles with \code{\link{ensemble_create}}.
#'   The provided rescaling methods are:
#'   \itemize{
#'     \item'abundance' - Rescale the density values so that the predicted abundance is \code{y.abund}
#'     \item'sumto1' - Rescale the density values so their sum is 1
#'   }
#'
#'   SDM uncertainty values must be rescaled differently than the prediction values.
#'   Columns specified in \code{x.var.idx} must contain variance values.
#'   These values will be rescaled using the formula \code{var(c * x) = c^2 * var(x)},
#'   where \code{c} is the rescaling factor for the associated predictions.
#'
#'   If \code{x.var.idx} is not \code{NULL}, then the function assumes
#'   \code{x.var.idx[1]} contains the variance values associated with the predictions in \code{x.idx[1]},
#'   \code{x.var.idx[2]} contains the variance values associated with the predictions in \code{x.idx[2]}, etc.
#'   Use \code{NA} in \code{x.var.idx} to indicate a set of predictions that does not have
#'   associated uncertainty values (e.g., \code{x.var.idx = c(4, NA, 5)})
#'
#' @return The \code{sf} object \code{x} with the columns specified by \code{x.idx} and \code{x.var.idx} rescaled.
#'   The \code{agr} attributes of \code{x} will be conserved
#'
#' @examples
#' ensemble_rescale(preds.1, c("Density", "Density2"), "abundance", 50)
#' ensemble_rescale(preds.1, c(1, 2), "sumto1")
#'
#' ensemble_rescale(
#'   preds.1, c("Density", "Density2"), "abundance", 100, c(3,4)
#' )
#'
#'
#' @export
ensemble_rescale <- function(x, x.idx, y, y.abund = NULL, x.var.idx = NULL) {
  #----------------------------------------------------------------------------
  # Check inputs

  #--------------------------------------------------------
  ### General checks
  stopifnot(
    inherits(x, "sf"),
    length(x.idx) < ncol(x)
  )

  #--------------------------------------------------------
  ### Check/process x.idx
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
  ### Check/process x.var.idx
  if (!is.null(x.var.idx)) {
    if (length(x.idx) != length(x.var.idx)) {
      stop("If specified, x.var.idx must be the same length as x.idx. ",
           "Use 'NA' in in x.var.idx for predictions that ",
           "do not have uncertainty values")
    }

    if (inherits(x.var.idx, "character")) {
      if (!all(x.var.idx %in% c(names(x.df), NA))) {
        stop("If x.var.idx is a character vector, then all elements of x.var.idx must ",
             "be the name of a column of x (not including the geometry list-column)")
      }

    } else if (inherits(x.var.idx, c("integer", "numeric"))) {
      if (!(max(x.var.idx, na.rm = TRUE) < ncol(x))) {
        stop("If x.var.idx is a numeric vector, then all values of x must be ",
             "less than ncol(x)")
      }
      x.var.idx <- names(x)[x.var.idx]

    } else {
      stop("If it is not NULL, x.var.idx must be a vector of class ",
           "character, integer, or numeric")
    }
  }

  # Check that x.idx and x.var.idx do not share any elements
  if (any(x.idx %in% x.var.idx))
    stop("x.idx and x.var.idx cannot point to any of the same columns of x")

  #--------------------------------------------------------
  ### Check/process y
  if (!(y %in% c("abundance", "sumto1"))) {
    stop("y must be either 'abundance' or 'sumto1'")
  }

  #----------------------------------------------------------------------------
  # Rescale values

  #--------------------------------------------------------
  ### Prep
  # Use select rather than [,] to ensure object is data frame
  x.df.idx <- x.df %>% select(!!x.idx)
  # x.df.var.idx <- x.df %>% select(!x.var.idx)
  x.df.var.idx <- data.frame(lapply(x.var.idx, function(i) {
    if (is.na(i)) NA else x.df %>% select(!!i)
  }))

  #--------------------------------------------------------
  ### Get rescaling factors
  if (y == "abundance") {
    if (!length(y.abund) == 1)
      stop ("If y is 'abundance', y.abund must be a single number greater than 0")
    if (!(y.abund > 0 & inherits(y.abund, c("integer", "numeric"))))
      stop ("If y is 'abundance', y.abund must be a single number greater than 0")


    d <- vapply(x.df.idx, function(i, j, k) {
      tmp.sf <- st_sf(pred = i, geometry = j, agr = "constant")
      eSDM::model_abundance(tmp.sf, "pred") / k
    }, 1, j = x.geom, k = y.abund, USE.NAMES = FALSE)

  } else { #y == "sumto1"
    d <- vapply(x.df.idx, sum, 1, na.rm = TRUE, USE.NAMES = FALSE)
  }

  #--------------------------------------------------------
  ### Replace original values with rescaled values
  x.df.idx.rescaled     <- map2_df(x.df.idx, d, function(i, j) i / j)
  x.df[, x.idx] <- x.df.idx.rescaled

  if (!is.null(x.var.idx)) {
    x.df.var.idx.rescaled <- map2_df(x.df.var.idx, d, function(i, j) i / (j^2)) #%>% select(which(!is.na(x.var.idx)))
    x.df[, x.var.idx] <- x.df.var.idx.rescaled
  }
  st_sf(x.df, geometry = x.geom, agr = st_agr(x))
}
