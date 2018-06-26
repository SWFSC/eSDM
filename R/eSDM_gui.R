#' eSDM GUI
#'
#' The eSDM graphical user interface (GUI; a Shiny app) for creating ensemble
#'   predictions using predictions from SDMs
#'
#' @usage eSDM_GUI()
#'
#' @importFrom shiny runApp
#'
#' @source https://swoodman.shinyapps.io/eSDM/
#'
#' @export
eSDM_GUI <- function() {
  appDir <- system.file("shiny", package = "eSDM")
  if (appDir == "") {
    stop("The eSDM GUI folder could not be found. Try re-installing 'eSDM'",
         call. = FALSE)
  }
  runApp(appDir, display.mode = "normal")
}
