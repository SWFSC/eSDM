#' Create sfc_polygon object from data.frame with polygon coordinates
#'
#' Create sfc_polygon object from data.frame with only longitude and
#'   latitude columns, respectively, which contain the polygon coordinates
#'
#' @param x data.frame with two columns; the columns must contain longitude
#'   and latitude coordinates, respectively
#' @param crs.prov crs value of new sfc_polygon object;
#'   value must be interpretable by st_crs(), see st_crs() documentation
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom sf st_polygon
#' @importFrom sf st_sfc
#'
#' @examples
#' x <- data.frame(
#'   lon = c(40, 40, 50, 50, 40, NA, 20, 20, 30, 30, 20),
#'   lat = c(0, 10, 10, 0, 0, NA, 0, 10, 10, 0, 0)
#' )
#' pts_to_sfc_coords(x, 4326)
#'
#' @export
pts_to_sfc_coords <- function(x, crs.prov = NULL) {
  stopifnot(
    inherits(x, "data.frame"),
    ncol(x) == 2
  )

  names(x) <- c("lon", "lat")
  if (anyNA(x$lon)) {
    obj.list <- x %>%
      mutate(na_sum = cumsum(is.na(lon) & is.na(lat))) %>%
      filter(!is.na(lon) & !is.na(lat)) %>%
      group_by(na_sum) %>%
      summarise(temp = list(
        st_polygon(list(matrix(c(.data$lon, .data$lat), ncol = 2)))
      ))

    if (is.null(crs.prov)) {
      st_sfc(obj.list$temp)
    } else {
      st_sfc(obj.list$temp, crs = crs.prov)
    }

  } else {
    if (is.null(crs.prov)) {
      st_sfc(st_polygon(list(as.matrix(x))))
    } else {
      st_sfc(st_polygon(list(as.matrix(x))), crs = crs.prov)
    }
  }
}


#' Create sfc_polygon object from data.frame with grid coordinates
#'
#' Create sfc_polygon object from data.frame with only longitude and
#'   latitude columns, respectively, which contain the coordinates
#'   of the center of each polygon of a grid
#'
#' @param x data.frame with at least two columns;
#'   the columns must contain longitude and latitude coordinates, respectively,
#'   and the coordinates must form a grid
#'

#'
#' @examples
#' x <- data.frame(
#'   lon = c(5, 10, 15, 20, 5, 10, 15, 20),
#'   lat = c(rep(5, 4), rep(12, 4))
#' )
#'
#' @export
pts_to_sfc_grid <- function(x) {
  NULL
}
