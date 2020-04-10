#' Create polygons from vertex coordinates
#'
#' Create polygon(s) from a data frame with the coordinates of the polygon vertices
#'
#' @param x data frame with at least two columns;
#'   the first two columns must contain longitude and latitude coordinates, respectively.
#'   See 'Details' section for how additional columns are handled
#' @param ... passed to \link[sf:sfc]{st_sfc},
#'   e.g. for passing named argument \code{crs}
#
#' @details Vertices of different polygons must be demarcated by rows with values of \code{NA}
#'   in both the first and second columns (i.e. the longitude and latitude columns).
#'
#'   All columns in \code{x} besides the first two columns are ignored.
#'
#'   If a \code{crs} is not specified in \code{...},
#'   then the \code{crs} attribute of the polygon(s) will be \code{NULL}.
#'
#' @return Object of class \code{sfc} with the geometry type \code{POLYGON}
#'
#' @examples
#' x <- data.frame(
#'   lon = c(40, 40, 50, 50, 40),
#'   lat = c(0, 10, 10, 0, 0)
#' )
#' pts2poly_vertices(x, crs = 4326)
#'
#' # Create an sf object
#' x <- data.frame(
#'   lon = c(40, 40, 50, 50, 40, NA, 20, 20, 30, 30, 20),
#'   lat = c(0, 10, 10, 0, 0, NA, 0, 10, 10, 0, 0)
#' )
#' sf::st_sf(Pred = 1:2, geometry = pts2poly_vertices(x, crs = 4326))
#'
#' @export
pts2poly_vertices <- function(x, ...) {
  stopifnot(
    inherits(x, "data.frame"),
    ncol(x) >= 2,
    identical(is.na(x[[1]]), is.na(x[[2]]))
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

    st_sfc(obj.list$temp, ...)

  } else {
    st_sfc(st_polygon(list(matrix(c(x$lon, x$lat), ncol = 2))), ...)
  }
}
