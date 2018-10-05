#' Open the eSDM GUI
#'
#' Open the eSDM graphical user interface (GUI); an R Shiny app for creating ensemble predictions using SDM predictions.
#'   The GUI can also be accessed at \url{https://swoodman.shinyapps.io/eSDM/}
#'
#' @param launch.browser Logical with default of \code{TRUE}; passed to \code{launch.browser} argument of \code{\link[shiny]{runApp}}
#'
#' @usage eSDM_GUI(launch.browser = TRUE)
#'
#' @importFrom shiny runApp
#'
#' @export
eSDM_GUI <- function(launch.browser = TRUE) {
  appDir <- system.file("shiny", package = "eSDM")
  if (appDir == "") {
    stop("The eSDM GUI folder could not be found. Try re-installing 'eSDM'",
         call. = FALSE)
  }
  runApp(appDir, launch.browser = launch.browser, display.mode = "normal")
}
