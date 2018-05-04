#' eSDM Shiny GUI
#'
#' Opens the actual eSDM tool interface
#'
#' @export

eSDM_Shiny <- function() {
  appDir <- system.file("shiny", package = "eSDM")
  if(appDir == "") {
    stop("shiny app folder could not be found. Try re-installing 'eSDM'", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
