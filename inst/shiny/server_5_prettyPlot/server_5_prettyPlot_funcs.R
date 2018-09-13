# Functions used in pretty plot section

###############################################################################
# Function to createa sfc object from vector of plot limits
#   x format: c(xmin, xmax, ymin, ymax)
pretty_plot_poly_func <- function(x, poly.crs) {
  stopifnot(length(x) == 4)

  poly.x <- x[c(1, 1, 2, 2, 1)]
  poly.y <- x[c(3, 4, 4, 3, 3)]
  range.poly <- st_sfc(
    st_polygon(list(cbind(poly.x, poly.y))), crs = poly.crs
  )
}


###############################################################################


###############################################################################
