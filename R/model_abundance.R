#' Calculate predicted abundance
#'
#' Calculates the predicted abundance by multiplying the values in the
#'   column(s) with density predictions by area of their prediciton polygon
#'   (calcualted using sf::st_area())
#'
#' @param x object of class sf; SDM with density predictions
#' @param dens.idx name or index of column in \code{x} with density predictions.
#'   Can be a vector of names or indices
#' @param sum.abund logical indicating whether to return the total predicted
#' abundnace (TRUE) or the abundance for for each prediction polygon (FALSE)
#'
#' @importFrom sf st_area

#' @return Either a single number (total predicted abundnace for the SDM) or
#'   a numerical vector with the predicted abundance for
#'   each prediction polygon
#'
#' @export
model_abundance <- function(x, dens.idx, sum.abund = TRUE) {
  x.area.m2 <- st_area(x)
  if (!all(units(x.area.m2)[[1]] == c("m", "m"))) {
    stop("Units of st_area(x) must be m^2")
  }
  x.area <- as.numeric(x.area.m2) / 1e+06

  sapply(dens.idx, function(j) {
    abund.vec <- as.data.frame(x)[j] * x.area
    if (sum.abund) sum(abund.vec, na.rm = TRUE) else abund.vec
  })
}
