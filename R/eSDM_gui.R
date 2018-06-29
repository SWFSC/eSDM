#' eSDM GUI
#'
#' The eSDM graphical user interface (GUI): a Shiny app for creating ensemble
#'   predictions using predictions from SDMs. You can also access the gui at
#'   https://swoodman.shinyapps.io/eSDM/
#'
#' @usage eSDM_GUI()
#'
#' @importFrom shiny runApp
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
