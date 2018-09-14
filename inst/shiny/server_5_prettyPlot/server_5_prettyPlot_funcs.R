# Functions used in pretty plot section

###############################################################################
# Create a sfc object from vector of plot limits
#   x format: c(xmin, xmax, ymin, ymax)
pretty_range_poly_func <- function(x, poly.crs) {
  stopifnot(length(x) == 4)

  poly.x <- x[c(1, 1, 2, 2, 1)]
  poly.y <- x[c(3, 4, 4, 3, 3)]
  range.poly <- st_sfc(
    st_polygon(list(cbind(poly.x, poly.y))), crs = poly.crs
  )
}


###############################################################################
# Check if x is completely within y, and if not then clip x by y
pretty_int_func <- function(x, y) {
  UseMethod("pretty_int_func")
}

pretty_int_func.sf <- function(x, y) {
  cover <- suppressMessages(st_covers(y, x))

  if (length(cover[[1]]) == nrow(x)) {
    x
  } else {
    suppressMessages(st_intersection(x, y))
  }
}

pretty_int_func.sfc <- function(x, y) {
  cover <- suppressMessages(st_covers(y, x))

  if (length(cover[[1]]) == length(x)) {
    x
  } else {
    suppressMessages(st_intersection(x, y))
  }
}


###############################################################################
#


###############################################################################
