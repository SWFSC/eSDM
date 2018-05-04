


#' @title Read GIS shpaefile from Shiny fileInput
#' @description Read in a GIS shapefile from a fileInput ouptput in a Shiny app
#'
#' @param file.in.list The list returned by shiny::fileInput()
#'
#' @return A sf object
#'
#' @examples
#'
#' @source \url{https://github.com/leonawicz/nwtapp/blob/master/mod_shpPoly.R}
#'
#' @export

shiny.read.shp <- function(file.in.list) {
  infiles <- file.in.list$datapath
  dir <- unique(dirname(infiles))
  outfiles <- file.path(dir, file.in.list$name)
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))

  gis.file <- try(st_read(dir, strsplit(file.in.list$name[1], "\\.")[[1]][1],
                          quiet = TRUE),
                  silent = TRUE)

  return(gis.file)
}
