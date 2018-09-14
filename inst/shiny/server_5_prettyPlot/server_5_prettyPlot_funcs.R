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
# Calculate color scheme data breaks and legend labels
pretty_colorscheme_func <- function(x, data.name, map.range, perc, color.num,
                                    leg.perc.esdm, leg.round) {

  y <- pretty_range_poly_func(map.range, st_crs(x))
  x <- pretty_int_func(x, y)
  x.df <- st_set_geometry(x, NULL)[, data.name]


  if (perc) {
    # Percentages
    data.breaks <- breaks_calc(x.df)
    labels.lab.pretty <- leg.perc.esdm

  } else {
    # Values
    data.breaks <- seq(
      min(x.df, na.rm = TRUE), max(x.df, na.rm = TRUE),
      length.out = (color.num + 1)
    )
    data.breaks.labs <- round(data.breaks, leg.round)
    labels.lab.pretty <- paste(
      format(head(data.breaks.labs, -1), nsmall = leg.round),
      format(tail(data.breaks.labs, -1), nsmall = leg.round),
      sep = " - "
    )
  }

  list(data.breaks, labels.lab.pretty)
}


###############################################################################
