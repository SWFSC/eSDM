#' Create ensemble of SDM predictions
#'
#' Create a weighted or unweighted ensemble of SDM predictions, including associated uncertainty values
#'
#' @param x object of class \code{sf} or class \code{data.frame}
#' @param x.idx vector of column names or numerical indices;
#'   indicates which columns in \code{x} will be used to create the ensemble
#' @param w weights for the ensemble; either a numeric vector the same length as \code{x} or
#'   a data frame (or tibble) with the same number of rows as \code{x} and \code{ncol(w) == length(x.idx)}.
#'   If w is a numeric vector, its values (i.e. the weights) must sum to 1.
#'   The default value is \code{1 / length(x.idx)}, i.e. an unweighted ensemble
#' @param x.var.idx vector of column names or column indices;
#'   indicates columns in \code{x} with variance values with which to
#'   calculate uncertainty values for the ensemble.
#'   If \code{x.var.idx} is specified, it must be the same length as \code{x.idx}.
#'   Use \code{x.var.idx = NULL} (the default) if none of the predictions have associated uncertainty values;
#'   in this case the uncertainty values for the ensemble will be calculated using the among-model uncertainty.
#'   See the 'Details' section for more information
#' @param ... Arguments to be passed to methods; specifically designed for passing
#'   \code{na.rm} argument to \code{sum}
#'
#' @details \code{ensemble_create} is designed to be used after overlaying predictions with \code{\link{overlay_sdm}} and
#'   (if desired) rescaling the overlaid predictions with \code{\link{ensemble_rescale}}.
#'
#'   This function implements ensemble methods provided in \link{eSDM_GUI}.
#'   Note that it does not implement regional exclusion, which must be done manually if not using the GUI.
#'
#'   Ensemble uncertainty is calculated using either the within-model uncertainty (if \code{x.var.idx} is specified) or
#'   the among-model uncertainty (if \code{x.var.idx} is \code{NULL}).
#'   See the eSDM GUI manual for applicable formulas.
#'
#' @return An object of the same class as \code{x} with two columns appended to the data frame:
#'   \itemize{
#'     \item 'Pred_ens' - The ensemble predictions
#'     \item 'Var_ens' - The variance of the ensemble predictions,
#'       calculated using either the within-model uncertainty (if \code{x.var.idx} is specified) or
#'       the among-model uncertainty (if \code{x.var.idx} is \code{NULL})
#'   }
#'   Note that all other columns of \code{x} will be included in the returned object.
#'   Also, if \code{x} is of class \code{sf} then
#'   1) the geometry list-column will be the last column of the returned object and
#'   2) the \code{agr} attribute will be set as 'constant' for 'Pred_ens' and 'Var_ens'
#'
#' @examples
#' ensemble_create(preds.1, c("Density", "Density2"), c(0.2, 0.8))
#' ensemble_create(preds.1, 1:2, c(0.2, 0.8), c("Var1", "Var2"))
#' ensemble_create(data.frame(a = 1:5, b = 3:7), c(1, 2))
#'
#' weights.df <- data.frame(runif(325), c(rep(NA, 100), runif(225)))
#' ensemble_create(preds.1, c("Density", "Density2"), weights.df, na.rm = TRUE)
#'
#' @export
ensemble_create <- function(x, x.idx, w = NULL, x.var.idx = NULL, ...) UseMethod("ensemble_create")


#' @export
#' @name ensemble_create
ensemble_create.sf <- function(x, x.idx, w = NULL, x.var.idx = NULL, ...) {
  st_sf(
    ensemble_create(st_set_geometry(x, NULL), x.idx, w, x.var.idx, ...),
    geometry = st_geometry(x),
    agr = c(as.character(st_agr(x)), "constant", "constant")
  )
}

#' @export
#' @name ensemble_create
ensemble_create.data.frame <- function(x, x.idx, w = NULL, x.var.idx = NULL, ...) {
  #----------------------------------------------------------------------------
  stopifnot(inherits(x, "data.frame"), length(x.idx) <= ncol(x))
  lst <- list(...)
  z.lgl <- if ("na.rm" %in% names(lst)) lst$na.rm else TRUE

  #--------------------------------------------------------
  # Check and process x.idx (vector of indicies to be used in ensemble)
  x.df <- x
  if (inherits(x.idx, "character")) {
    if (!all(x.idx %in% names(x.df)))
      stop("If x.idx is a character vector, then all elements of x.idx must ",
           "be the name of a column of x")

  } else if (inherits(x.idx, c("integer", "numeric"))) {
    if (!(max(x.idx) <= ncol(x.df)))
      stop("If x.idx is a numeric vector, then all values of x must be ",
           "less than or equal to ncol(x)")

    x.idx <- names(x)[x.idx]

  } else {
    stop("x.idx must be a vector of class character, integer, or numeric")
  }

  #--------------------------------------------------------
  ### Check/process x.var.idx
  if (!is.null(x.var.idx)) {
    if (length(x.idx) != length(x.var.idx))
      stop("If specified, x.var.idx must be the same length as x.idx")

    if (inherits(x.var.idx, "character")) {
      if (!all(x.var.idx %in% c(names(x.df))))
        stop("If x.var.idx is a character vector, then all elements of x.var.idx must ",
             "be the name of a column of x (not including the geometry list-column)")

    } else if (inherits(x.var.idx, c("integer", "numeric"))) {
      if (!(max(x.var.idx, na.rm = TRUE) <= ncol(x)))
        stop("If x.var.idx is a numeric vector, then all values of x must be ",
             "less than or equal to ncol(x)")

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
  # Check and process w (weights)
  if (is.null(w)) {
    w <- rep(1 / length(x.idx), length(x.idx))

  } else if (inherits(w, "numeric")) {
    if (length(x.idx) != length(w)) stop("x.idx and w must have the same length")
    if (!all.equal(sum(w), 1)) stop("If w is a numeric vector, it must sum to 1")

  } else if (inherits(w, "data.frame")) {
    stopifnot(ncol(w) == length(x.idx), nrow(x) == nrow(w))
    # Normalize (make sum to 1) each row of w
    w <- data.frame(t(apply(w, 1, function(i, z) i / sum(i, na.rm = z), z = z.lgl)))

  } else {
    stop("w must be one of NULL, a data frame (or tibble), or a numeric vector")
  }


  #----------------------------------------------------------------------------
  # Create ensemble and calculate SE values
  x.df.idx   <- x.df %>% select(!!x.idx)
  if (!is.null(x.var.idx)) x.var.df.idx <- x.df %>% select(!!x.var.idx)

  #----------------------------------------------
  if (inherits(w, "numeric")) {
    ### Ensemble prediction values
    data.ens <- unname(apply(x.df.idx, 1, esdm_weighted_mean, w = w, ... = z.lgl))

    ### Ensemble uncertainty values (variance)
    if (is.null(x.var.idx)) {
      # Among-model variance
      data.ens.var <- unname(apply(
        cbind(x.df.idx, data.ens), 1, function(i, j, z) {
          esdm_weighted_var_amv(head(i, -1), tail(i, 1), j, na.rm = z)
        }, j = w, z = z.lgl
      ))

    } else {
      # Within-model variance
      data.ens.var <- unname(apply(
        x.var.df.idx, 1, function(i, j, z) {
          esdm_weighted_var_wmv(i, j, na.rm = z)
        }, j = w, z = z.lgl
      ))
    }

    #--------------------------------------------
  } else if (inherits(w, "data.frame")) {
    ### Ensemble prediction values
    data.ens <- unname(apply(cbind(x.df.idx, w), 1, function(i, k, z) {
      esdm_weighted_mean(head(i, k), tail(i, k), na.rm = z)
    }, k = ncol(w), z = z.lgl))

    ### Ensemble uncertainty values (variance)
    if (is.null(x.var.idx)) {
      # Among-model variance
      data.ens.var <- unname(apply(cbind(x.df.idx, w, data.ens), 1, function(i, k, z) {
        i.mean <- tail(i, 1)
        i <- head(i, -1)
        esdm_weighted_var_amv(head(i, k), i.mean, tail(i, k), na.rm = z)
      }, k = ncol(w), z = z.lgl))

    } else {
      # Within-model variance
      data.ens.var <- unname(apply(
        cbind(x.var.df.idx, w), 1, function(i, k, z) {
          esdm_weighted_var_wmv(head(i, k), tail(i, k), na.rm = z)
        }, k = ncol(w), z = z.lgl
      ))
    }

    #--------------------------------------------
  } else {
    stop("Error in ensemble_create()")
  }

  stopifnot(is.numeric(data.ens), is.numeric(data.ens.var))
  data.ens[is.nan(data.ens)] <- NA
  data.ens.var[is.nan(data.ens.var)] <- NA

  data.frame(x, Pred_ens = data.ens, Var_ens = data.ens.var)
}
