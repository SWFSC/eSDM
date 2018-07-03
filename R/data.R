#' Low resolution GSHHG world map
#'
#' Low resolution GSHHG world map, includes heirarchial levels
#'   L1 and L6. Processed using lwgeom::st_make_valid() to make the
#'   polygon valid
#'
#' @format An object with class c("sfc_GEOMETRY", "sfc"):
#' @source \url{http://www.soest.hawaii.edu/pwessel/gshhg/}
"gshhg.l.L16"

#' Sample SDM density predictions
#'
#' Sample SDM density predictions; created by importing
#'   Sample_predictions_2.csv into the GUI, exporting predictions, and then
#'   clipping them to the SoCal_bite.csv region
#'
#' @format An object with class c("data.frame", "sf")
"preds.1"

#' Sample SDM density predictions
#'
#' Sample SDM density predictions; created by importing
#'   Sample_predictions_1.csv into the GUI, exporting predictions, and then
#'   clipping them to the SoCal_bite.csv region
#'
#' @format An object with class c("data.frame", "sf")
"preds.2"

#' Sample SDM density predictions
#'
#' Sample SDM density predictions; created by importing
#'   Sample_predictions_4_gdb into the GUI, exporting predictions, and then
#'   clipping them to the SoCal_bite.csv region
#'
#' @format An object with class c("data.frame", "sf")
"preds.3"

#' Sample validation data points
#'
#' Sample SDM predictions; created from Validation_data.csv clipped to the
#'   SoCal_bite.csv region
#'
#' @format An object with class c("data.frame", "sf")
"validation.data"
