#' Create polygon from polygon coordinates
#'
#' Create object of class \code{sfc} of type \code{POLYGON} from a data frame with the polygon coordinates
#'
#' @param x data frame with two columns;
#'   the columns must contain longitude and latitude coordinates, respectively
#' @param crs.prov crs value for polygon being created;
#'   value must be readable by \code{\link{sf}{st_crs}}
#
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom purrr set_names
#' @importFrom rlang .data
#' @importFrom sf st_polygon
#' @importFrom sf st_sfc
#'
#' @details TODO
#'
#' @return Object of class \code{sfc} of type \code{POLYGON} with the provided coordinates and crs value
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
    ncol(x) >= 2
  )

  x <- x %>%
    select(c(1, 2)) %>%
    set_names(c("lon", "lat"))

  if (anyNA(x$lon)) {
    obj.list <- x %>%
      mutate(na_sum = cumsum(is.na(.data$lon) & is.na(.data$lat))) %>%
      filter(!is.na(.data$lon) & !is.na(.data$lat)) %>%
      group_by(.data$na_sum) %>%
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


#' Create polygons from grid coordinates
#'
#' Create object of class \code{sfc} of type \code{POLYGON} from a data frame with
#'   coordinates of the center of each polygon of a grid
#'
#' @param x data frame with at least two columns;
#'   the first two columns must contain longitude and latitude coordinates, respectively,
#'   and the coordinates must form a grid
#' @param poly.radius radius of each polygon to be created around points in \code{x};
#'   must be numeric object
#' @param crs.prov crs value of new sfc_polygon object;
#'   value must be interpretable by st_crs(), see st_crs() documentation
#'
#' @importFrom sf st_polygon
#' @importFrom sf st_sfc
#'
#' @details TODO
#'
#' @return Object of class \code{sfc} of type \code{POLYGON}
#'
#' @examples
#' x <- data.frame(
#'   lon = c(5, 10, 15, 20, 5, 10, 15, 20),
#'   lat = c(rep(5, 4), rep(10, 4))
#' )
#' pts_to_sfc_grid(x, 2.5, 4326)
#'
#' @export
pts_to_sfc_grid <- function(x, poly.radius, crs.prov = NULL) {
  stopifnot(
    inherits(x, "data.frame"),
    ncol(x) >= 2,
    inherits(poly.radius, "numeric")
  )

  sfc.list <- apply(x, 1, function(i, j) {
    i <- as.numeric(i)
    st_sfc(st_polygon(list(matrix(
      c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
        i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
      ncol = 2
    ))))
  }, j = poly.radius)

  if (is.null(crs.prov)) {
    st_sfc(do.call(rbind, sfc.list))
  } else {
    st_sfc(do.call(rbind, sfc.list), crs = crs.prov)
  }
}
