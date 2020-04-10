#' Create polygons from centroid coordinates
#'
#' Create polygon(s) from a data frame with coordinates of the polygon centroid(s)
#'
#' @param x data frame with at least two columns;
#'   the first two columns must contain longitude and latitude coordinates, respectively.
#'   See 'Details' section for how additional columns are handled
#' @param y numeric; the perpendicular distance from the polygon centroid (center) to its edge
#'   (i.e. half the length of one side of a polygon)
#' @param ... passed to \link[sf:sf]{st_sf} or to \link[sf:sfc]{st_sfc},
#'   e.g. for passing named arguments \code{crs} and \code{agr}
#'
#' @details This function was designed for someone who reads in a .csv file
#'   with a grid of coordinates representing SDM prediction points and needs to create
#'   prediction polygons with the .csv file coordinates as the polygon centroids.
#'   However, the function can be used to create square polygons of any size around the provided points,
#'   regardless of if those polygons touch or overlap.
#'   The created polygons are oriented so that, in a 2D plane, their edges are parallel to either the x or the y axis.
#'
#'   If \code{x} contains more than two column, then additional columns will be treated as simple feature attributes,
#'   i.e. passed along as the first argument to \link[sf:sf]{st_sf}
#'
#'   If a \code{crs} is not specified in \code{...},
#'   then the \code{crs} attribute of the polygon(s) will be \code{NULL}.
#'
#' @return Object of class \code{sfc} (if \code{x} has exactly two columns) or class \code{sf}
#'   (if \code{x} has exactly more than two columns). The object will have a geometry type of \code{POLYGON}.
#'   If the object is of class \code{sf}, the name of the geometry list-column will be "geometry"
#'
#' @examples
#' # Create an sfc object from a data frame of two columns
#' x <- data.frame(
#'   lon = c(5, 10, 15, 20, 5, 10, 15, 20),
#'   lat = c(5, 5, 5, 5, 10, 10, 10, 10)
#' )
#' pts2poly_centroids(x, 2.5, crs = 4326)
#'
#' # Create an sf object from a data frame of more than two columns
#' x <- data.frame(
#'   lon = c(5, 10, 15, 20, 5, 10, 15, 20),
#'   lat = c(5, 5, 5, 5, 10, 10, 10, 10),
#'   sdm.pred = runif(8),
#'   sdm.pred2 = runif(8)
#' )
#' pts2poly_centroids(x, 2.5, crs = 4326, agr = "constant")
#'
#' @export
pts2poly_centroids <- function(x, y, ...) {
  # Input checks
  stopifnot(
    inherits(x, "data.frame"),
    ncol(x) >= 2,
    is.numeric(y)
  )

  if (ncol(x) == 2 & ("agr" %in% names(list(...))))
    stop("agr cannot be passed to st_sfc(), ",
         "meaning when x only has two columns")


  # Use first two (lon and lat) columns to create list of sfg objects
  x.lonlat <- x %>%
    select(c(1, 2)) %>%
    set_names(c("lon", "lat"))

  sfg.list <- unname(apply(x.lonlat, 1, function(i, j) {
    st_polygon(list(matrix(
      c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
        i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
      ncol = 2
    )))
  }, j = y))

  # Create sf or sfc object, as appropriate
  if (ncol(x) > 2) {
    x %>%
      select(-c(1, 2)) %>%
      st_sf(geometry = st_sfc(sfg.list), ...)
  } else {
    st_sfc(sfg.list, ...)
  }
}
