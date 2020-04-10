#' Low resolution GSHHG world map
#'
#' Low resolution GSHHG world map, includes hierarchical levels
#'   L1 and L6. Processed using \code{\link[sf:valid]{st_make_valid}}
#'
#' @format An object of class \code{\link[sf]{sfc}}
#' @source \url{http://www.soest.hawaii.edu/pwessel/gshhg/}
"gshhg.l.L16"

#' Sample SDM density predictions
#'
#' \code{preds.1}, \code{preds.2}, and \code{preds.3} are objects of class \code{\link[sf]{sf}} that serve as
#'   sample sets of SDM density predictions for the \code{eSDM} package
#'
#' @details
#' \code{preds.1} sample SDM density predictions created by importing
#'   Sample_predictions_2.csv into the eSDM GUI, exporting predictions, and then
#'   clipping them to the SoCal_bite.csv region.
#'   Also manually added two variance columns (numbers are randomly generated with a max of 0.01)
#'
#' \code{preds.2} sample SDM density predictions created by importing
#'   Sample_predictions_1.csv into the eSDM GUI, exporting predictions, and then
#'   clipping them to the SoCal_bite.csv region
#'
#' \code{preds.3} is a set of sample SDM density predictions created by importing
#'   Sample_predictions_4_gdb into the eSDM GUI, exporting predictions, and then
#'   clipping them to the SoCal_bite.csv region
#'
#' @format Objects of class \code{sf} with a column of density predictions (name: \code{Density}) and
#'   a simple feature list column (name: \code{geometry}).
#'   \code{preds.1} also has a second column of sample density predictions (name: \code{Density2}),
#'   as well as \code{Var1} and \code{Var2}, representing the variance
#'
#'   \code{preds1}: An object of class sf (inherits from data.frame) with 325 rows and 5 columns.
#'
#'   \code{preds2}: An object of class sf (inherits from data.frame) with 1891 rows and 2 columns.
#'
#'   \code{preds3}: An object of class sf (inherits from data.frame) with 1445 rows and 2 columns.
#'
#' @name preds
#' @aliases preds.1 preds.2 preds.3


#' @rdname preds
#' @format
"preds.1"

#' @rdname preds
#' @format
"preds.2"

#' @rdname preds
#' @format
"preds.3"


#' Sample validation data
#'
#' Sample validation data created by cropping Validation_data.csv to the SoCal_bite.csv region
#'   (.csv files from ...)
#'
#' @format An object of class \code{\link[sf]{sf}} with 8 rows and 3 variables
#' \describe{
#'   \item{sight}{1's and 0's indicating species presence/absence}
#'   \item{count}{number of individuals observed at each point}
#'   \item{geometry}{simple feature list column representing validation data points}
#' }
"validation.data"
