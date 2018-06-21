#' eSDM GUI
#'
#' The eSDM graphical user interface (a shiny app) for creating ensembles
#'   using predictions from SDMs
#'
#' @usage eSDM_gui()
#'
#' @source https://swoodman.shinyapps.io/eSDM/
#'
#' @export
eSDM_gui <- function() {
  appDir <- system.file("shiny", package = "eSDM")
  if (appDir == "") {
    stop("shiny app folder could not be found. Try re-installing 'eSDM'",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
