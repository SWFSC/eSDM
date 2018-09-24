#' Create polygons from vertice coordinates
#'
#' Create polygon(s) from a data frame with the coordinates of the polygon vertices
#'
#' @param x data frame with at least two columns;
#'   the first two columns must contain longitude and latitude coordinates, respectively.
#'   All other columns are ignored
#' @param crs.prov crs value for polygon being created;
#'   value must be readable by \code{\link[sf]{st_crs}}
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
#' @details Vertices of different polygons must be demarcated by rows with values of \code{NA}
#'   in both the first and second columns (i.e. the longitude and latitude columns).
#'
#'   If \code{crs.prov} is not specified, then the \code{crs} attribute of the polygon(s) will be \code{NULL}.
#'
#' @return Object of class \code{sfc} of type \code{POLYGON} with the provided crs
#'
#' @examples
#' x <- data.frame(
#'   lon = c(40, 40, 50, 50, 40, NA, 20, 20, 30, 30, 20),
#'   lat = c(0, 10, 10, 0, 0, NA, 0, 10, 10, 0, 0)
#' )
#' pts_to_sfc_vertices(x, 4326)
#'
#' @export
pts_to_sfc_vertices <- function(x, crs.prov = NULL) {
  stopifnot(
    inherits(x, "data.frame"),
    ncol(x) >= 2,
    identical(is.na(x[, 1]), is.na(x[, 2]))
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



#' Create polygons from centroid coordinates
#'
#' Create polygon(s) from a data frame with coordinates of the polygon centroid(s)
#'
#' @inheritParams pts_to_sfc_vertices
#' @param y numeric; shortest distance from centroid to any of the sides,
#'   and thus also half the length of one side of the polygon
#'
#' @importFrom sf st_polygon
#' @importFrom sf st_sfc
#'
#' @details This function was designed for someone who reads in a .csv file
#'   with a grid of coordinates representing SDM prediction points and needs to create
#'   non-overlapping prediction polygons with the .csv file coordinates as the centroids.
#'   However, it can be used to create square polygons around any points, even if those polygons overlap.
#'
#'   The created polygons are oriented so that, in a 2D plane, their edges are parallel to either the x or the y axis.
#'
#'   If \code{crs.prov} is not specified, then the \code{crs} attribute of the polygon(s) will be \code{NULL}.
#'
#' @return Object of class \code{sfc} of type \code{POLYGON} with the provided crs
#'
#' @examples
#' x <- data.frame(
#'   lon = c(5, 10, 15, 20, 5, 10, 15, 20),
#'   lat = c(rep(5, 4), rep(10, 4))
#' )
#' pts_to_sfc_centroids(x, 2.5, 4326)
#'
#' # Combine data and created sfc object to make sf object
#' x <- data.frame(
#'   lon = c(5, 10, 15, 20, 5, 10, 15, 20),
#'   lat = c(5, 5, 5, 5, 10, 10, 10, 10),
#'   sdm.pred = runif(8),
#'   sdm.pred2 = runif(8)
#' )
#' x.sfc <- pts_to_sfc_centroids(x, 2.5, 4326)
#' x.sf <- sf::st_sf(x[, 3:4], x.sfc, agr = "constant")
#'
#' @export
pts_to_sfc_centroids <- function(x, y, crs.prov = NULL) {
  stopifnot(
    inherits(x, "data.frame"),
    ncol(x) >= 2,
    inherits(y, "numeric") | inherits(y, "integer")
  )

  sfc.list <- apply(x, 1, function(i, j) {
    i <- as.numeric(i)
    st_sfc(st_polygon(list(matrix(
      c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
        i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
      ncol = 2
    ))))
  }, j = y)

  if (is.null(crs.prov)) {
    st_sfc(do.call(rbind, sfc.list))
  } else {
    st_sfc(do.call(rbind, sfc.list), crs = crs.prov)
  }
}
