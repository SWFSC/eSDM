# Functions used in pretty plot section

###############################################################################
# Create a sfc object from vector of plot limits (e.g. output of st_bbox())
#   x format: c(xmin, xmax, ymin, ymax)
pretty_range_poly_func <- function(x, poly.crs) {
  stopifnot(length(x) == 4)

  poly.x <- x[c(1, 1, 2, 2, 1)]
  poly.y <- x[c(3, 4, 4, 3, 3)]

  st_sfc(
    st_polygon(list(cbind(poly.x, poly.y))), crs = poly.crs
  )
}


###############################################################################
# Check if x is completely within y, and if not then clip x by y
# tmap explanation
#   tmap can plot objects in [0, 360] range, but only if object has some
#     area in [0, 180] and 'bbox' arg of tm_shape() is specified
#   tmap can plot objects in (180, 360] range only when crs is NA,
#     although this does warnings to be printed. One warning can be taken care
#     of using 'projection' arg of tm_shape()
pretty_int_func <- function(x, y, x.name) {
  UseMethod("pretty_int_func")
}

pretty_int_func.sf <- function(x, y, x.name) {
  cover <- suppressMessages(st_covers(y, x))

  if (length(cover[[1]]) != nrow(x)) {
    x <- suppressMessages(st_intersection(x, y))
  }

  # Predictions will always be sf object
  if (identical(x.name, "selected predictions")) {
    val.message <- paste(
      "Error: None of the geometry of the", x.name,
      "is within the specified map range;",
      "adjust the map range to plot these predictions"
    )
  } else {
    val.message <- paste(
      "Error: None of the geometry of the", x.name,
      "is within the specified map range;",
      "either remove this object or adjust the map range"
    )
  }

  validate(
    need(nrow(x) > 0 && !inherits(st_geometry(x), "sfc_LINESTRING"),
         val.message)
  )

  x
}

pretty_int_func.sfc <- function(x, y, x.name) {
  cover <- suppressMessages(st_covers(y, x))

  if (length(cover[[1]]) != length(x)) {
    x <- suppressMessages(st_intersection(x, y))
  }

  val.message <- paste(
    "Error: None of the geometry of the", x.name,
    "is within the specified map range;",
    "either remove this object or adjust the map range"
  )

  validate(
    need(length(x) > 0 && !inherits(x, "sfc_LINESTRING"),
         val.message)
  )

  x
}


###############################################################################
# Set crs of objects as NA if their long range is (180, 360] dec deg
pretty_crsNA_func <- function(x) {
  y1 <- st_sfc(st_polygon(list(
    matrix(c(-180, 0, 0, -180, -180, -90, -90, 90, 90, -90), ncol = 2)
  )), crs = 4326)
  y1 <- st_transform(y1, st_crs(x))
  lon.180 <- abs(unname(st_bbox(y1))[1])

  if (st_bbox(x)[1] > lon.180) st_set_crs(x, NA) else x
}


###############################################################################
# Calculate color scheme data breaks and legend labels
pretty_colorscheme_func <- function(x, data.name, map.range, perc, color.num,
                                    leg.perc.esdm, leg.round) {
  # Clip predictions to map range
  y <- pretty_range_poly_func(map.range, st_crs(x))
  x <- pretty_int_func(x, y, "selected predictions")
  x.df <- st_set_geometry(x, NULL)[, data.name]

  # Get color scheme info for clipped predictions
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
