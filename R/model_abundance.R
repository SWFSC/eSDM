#' Calculate predicted abundance
#'
#' Calculates the predicted abundance by multiplying the density predictions by prediction polygon areas
#'
#' @param x object of class \code{sf}; SDM with density predictions
#' @param dens.idx name or index of column(s) in \code{x} with density predictions.
#'   Can be a character vector (column names) or numeric vector (column indices)
#' @param sum.abund logical; if \code{TRUE} return the total predicted abundance and
#'  if \code{FALSE} the abundance for for each prediction polygon
#'
#' @importFrom dplyr %>%
#' @importFrom purrr set_names
#' @importFrom sf st_area
#' @importFrom sf st_set_geometry
#'
#' @details Multiplies the values in the specified column(s) (i.e. the density predictions)
#'   by the area in square kilometers of their corresponding prediciton polygon.
#'   The area of each prediction polygon is calcualted using \code{st_area} from \code{\link[sf]{geos_measures}}
#'
#' @return Depends on \code{dens.idx} and \code{sum.abund}.
#'   If \code{dens.idx} is a vector of length 1, then ...TODO
#'   either a single number (total predicted abundance for the SDM) or
#'   a numerical vector with the predicted abundance for each prediction polygon
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
    inherits(dens.idx, c("character", "numeric")),
    inherits(sum.abund, "logical")
  )

  x.area <- st_area(x)
  if (!all(units(x.area)[[1]] == c("m", "m"))) {
    stop("Units of st_area(x) must be m^2; please ensure that x has a valid crs code")
  }
  x.area <- as.numeric(x.area) / 1e+06

  x.df <- st_set_geometry(x, NULL)

  if (sum.abund) {
    sapply(dens.idx, function(j) {sum(x.df[, j] * x.area, na.rm = TRUE)})

  } else {
    if (length(dens.idx) == 1) {
      x.df[, dens.idx] * x.area
    } else {
      data.frame(sapply(dens.idx, function(j) {x.df[, j] * x.area})) %>%
        set_names(paste0(dens.idx, ".abund"))
    }
  }
}
