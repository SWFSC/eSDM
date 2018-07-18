#' eSDM GUI
#'
#' The eSDM graphical user interface (GUI); an R Shiny app for creating ensemble predictions using SDM predictions.
#'   The GUI can also be accessed at \url{https://swoodman.shinyapps.io/eSDM/}
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
