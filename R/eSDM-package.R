#' eSDM: A tool for creating and exploring ensembles of predictions from Species Distribution Models
#'
#' eSDM provides functionality for overlaying SDM predictions onto a single base geometry
#'   and creating and evaluating ensemble predictions.
#'   This can be done manually in R, or using the eSDM GUI (an R Shiny app) opened through \link{eSDM_GUI}
#'
#' @details eSDM allows users to overlay SDM predictions onto a single base geometry,
#'   create ensembles of these predictions via weighted or unweighted averages,
#'   calculate performance metrics for each set of predictions and for resulting ensembles,
#'   and visually compare ensemble predictions with original predictions.
#'   The information provided by this tool can assist users in understanding spatial uncertainties and
#'   making informed conservation decisions.
#'
#'   The GUI ensures that the tool is accessible to non-R users,
#'   while also providing a user-friendly environment for functionality
#'   such as loading other polygons to use and visualizing predictions.
#'   However, user choices are restricted to the workflow provided by the GUI.
#'
#' @name eSDM-package
#' @aliases eSDM
#' @title Ensemble tool for predictions from Species Distribution Models
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @seealso \url{https://smwoodman.github.io/eSDM/}
#'
#' @importFrom dplyr all_of arrange between bind_rows filter group_by left_join mutate rename select summarise
#' @importFrom magrittr %>%
#' @importFrom methods slot
#' @importFrom purrr map map2_df set_names
#' @importFrom rlang .data sym
#' @importFrom ROCR performance prediction
#' @importFrom sf st_agr st_set_agr st_area st_bbox st_crop st_crs st_geometry st_intersection st_intersects
#'   st_polygon st_set_geometry st_sf st_sfc st_is_valid st_make_valid
#' @importFrom shiny runApp
#' @importFrom stats na.omit
#' @importFrom units set_units
#' @importFrom utils head tail
#'
#' @keywords package
NULL
