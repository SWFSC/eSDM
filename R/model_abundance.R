#' Calculate predicted abundance
#'
#' Calculates the predicted abundance by multiplying the density prediction values by prediction polygon areas
#'
#' @param x object of class \code{sf}; SDM with density predictions.
#'   Must have a valid crs code
#' @param dens.idx name or index of column(s) in \code{x} with density predictions.
#'   Can be a character vector (column names) or numeric vector (column indices)
#' @param sum.abund logical; whether or not to sum all of the predicted abundances
#'
#' @details Multiplies the values in the specified column(s) (i.e. the density predictions)
#'   by the area in square kilometers of their corresponding prediction polygon.
#'   The area of each prediction polygon is calculated using \code{st_area} from \code{\link[sf]{geos_measures}}.
#'   x must have a valid crs code to calculate area for these abundance calculations.
#'
#' @return If \code{sum.abund == TRUE}, then a vector of the same length as \code{dens.idx}
#'   representing the predicted abundance for the density values in each column.
#'
#'   If \code{sum.abund == FALSE} and the length of \code{dens.idx} is 1,
#'   then a numeric vector with the predicted abundance of each prediction polygon of \code{x}.
#'
#'   If \code{sum.abund == FALSE} and the length of \code{dens.idx} is greater than 1,
#'   then a data frame with \code{length(dens.idx)} columns of the predicted abundance of prediction polygons
#'
#' @examples
#' model_abundance(preds.1, "Density")
#' model_abundance(preds.1, c(1, 1))
#' model_abundance(preds.1, c(1, 1), FALSE)
#'
#' @export
model_abundance <- function(x, dens.idx, sum.abund = TRUE) {
  stopifnot(
    inherits(x, "sf"),
    inherits(dens.idx, "character") | inherits(dens.idx, "numeric"),
    inherits(sum.abund, "logical")
  )

  if (is.na(st_crs(x))) stop("x must have a valid crs code")

  x.area <- st_area(x)
  if (!all(units(x.area)[[1]] == c("m", "m"))) {
    stop("Units of st_area(x) must be m^2; please ensure that x has a valid crs code")
  }
  x.area <- as.numeric(x.area) / 1e+06

  x.df <- st_set_geometry(x, NULL)

  if (sum.abund) {
    sapply(dens.idx, function(j) {sum(x.df[[j]] * x.area, na.rm = TRUE)})

  } else {
    if (length(dens.idx) == 1) {
      x.df[, dens.idx] * x.area
    } else {
      data.frame(sapply(dens.idx, function(j) {x.df[[j]] * x.area})) %>%
        set_names(paste0(dens.idx, ".abund"))
    }
  }
}
