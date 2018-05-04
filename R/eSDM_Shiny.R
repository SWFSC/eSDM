#' eSDM Shiny GUI
#'
#' A graphical user interface for running the eSDM.
#'
#'
#'
#' @export

eSDM_Shiny <- function() {
  appDir <- system.file("shiny", package = "eSDM")
  if(appDir == "") {
    stop("shiny app folder could not be found. Try re-installing 'eSDM'", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
