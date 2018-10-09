###############################################################################
# [0, 360] - related functions

# All inspired by https://github.com/r-spatial/sf/issues/280
# preview360_split(): Converts spatial object to range [0, 360] by splitting
#   polygons that span the antimeridian and adding 360 to longitude coordinates
#   of polygons in [-180, 0]. This function uses st_intersects(), so it should
#   only be used when no polygons span the prime meridian.
#
#   Main advantage: Fastest method
#   Disadvantage: Does not work with polygons that span prime meridian, and
#     does not conserve the number of polygons
#   Summary: Used for plotting objects other than world objects

# preview360_mod(): Converts spatial object to range [0, 360] using the
#   mod ('%%') function, and thus without splitting polygons that span the
#   antimeridian.
#
#   Main advantage: Conserves polygons that span the antimeridian;
#     best for exporting or union-ing single polygons that had been split
#     along the antimeridian
#   Disadvantage: Slower
#   Summary: Used for exporting objects

# preview360_split_intersection(): Converts spatial object to range [0, 360] by
#   splitting polygons that span the antimeridian and adding 360 to
#   longitude coordinates of polygons in [-180, 0].
#   This function uses st_intersection(), so it can
#   be used when polygons span the prime meridian, e.g. world polygons.
#
#   Main advantage: Can handle world polygons
#   Disadvantage: Does not conserve number of polygons
#   Summary: Used for land polygons


###############################################################################
#------------------------------------------------------------------------------
### Tests if x spans the antimeridian
check_360 <- function(x) {
  stopifnot(isTruthy(st_crs(x)[[2]]))

  x.bbox.lon <- round(unname(st_bbox(x)), 3)
  identical(abs(x.bbox.lon[1]), x.bbox.lon[3])
}


#------------------------------------------------------------------------------
### Top-level for converting antimeridian-spanning objects to 0-360 if nec
# If any objects span the meridian (0 decimal degrees),
#   then we have to use intersection rather than intersects or mod
#   or else the polygons will do a world / world2 thing
check_preview360_split <- function (x, force.360 = FALSE) {
  if (check_360(x) || force.360) {
    meridian <- st_sfc(
      st_linestring(x = matrix(c(0, 0, 90, -90), ncol = 2)), crs = 4326
    )
    meridian <- st_transform(meridian, st_crs(x))

    if (length(suppressMessages(st_intersects(meridian, x))[[1]]) > 0) {
      preview360_split_intersection(x)
    } else {
      preview360_split(x)
    }

  } else {
    x
  }
}


check_preview360_mod <- function (x, force.360 = FALSE) {
  if (check_360(x) || force.360) {
    meridian <- st_sfc(
      st_linestring(x = matrix(c(0, 0, 90, -90), ncol = 2)), crs = 4326
    )
    meridian <- st_transform(meridian, st_crs(x))

    if (length(suppressMessages(st_intersects(meridian, x))[[1]]) > 0) {
      preview360_split_intersection(x)
    } else {
      preview360_mod(x)
    }

  } else {
    x
  }
}


###############################################################################
#  See top of file for details
preview360_split <- function(x) {
  UseMethod("preview360_split")
}

#------------------------------------------------------------------------------
preview360_split.sf <- function(x) {
  x.agr <- st_agr(x)
  x.crs <- st_crs(x)

  if (inherits(st_geometry(x), "sfc_GEOMETRY")) {
    x <- st_cast(x)
  }

  if (inherits(st_geometry(x), "sfc_MULTIPOLYGON")) {
    x <- st_cast(x, "POLYGON", warn = FALSE)
  } else if (inherits(st_geometry(x), "sfc_MULTIPOINT")) {
    x <- st_cast(x, "POINT", warn = FALSE)
  }

  stopifnot(
    inherits(st_geometry(x), "sfc_POLYGON") | inherits(st_geometry(x), "sfc_POINT")
  )

  y <- st_sfc(st_polygon(list(
    matrix(c(-180, 0, 0, -180, -180, -90, -90, 90, 90, -90), ncol = 2)
  )), crs = 4326)
  y <- st_transform(y, st_crs(x))
  lon.add <- abs(unname(st_bbox(y))[1] * 2)

  y.x <- suppressMessages(st_intersects(y, x)[[1]])
  y.x.no <- (1:nrow(x))[-y.x]

  x.df <- st_set_geometry(x, NULL)
  x.geom <- st_geometry(x)

  x1 <- data.frame(x.df[y.x, ]) %>%
    purrr::set_names(names(x.df)) %>%
    st_sf(geometry = x.geom[y.x] + c(lon.add, 0), agr = x.agr, crs = x.crs)
  x2 <- data.frame(x.df[y.x.no, ]) %>%
    purrr::set_names(names(x.df)) %>%
    st_sf(geometry = x.geom[y.x.no], agr = x.agr)

  preview360.areacheck(x, x1, x2)

  st_set_agr(rbind(x1, x2)[order(c(y.x, y.x.no)), ], x.agr)
}

#------------------------------------------------------------------------------
preview360_split.sfc <- function(x) {
  x.crs <- st_crs(x)

  if (inherits(x, "sfc_GEOMETRY")) x <- st_cast(x)

  if (inherits(x, "sfc_MULTIPOLYGON")) {
    x <- st_cast(x, "POLYGON", warn = FALSE)
  } else if (inherits(x, "sfc_MULTIPOINT")) {
    x <- st_cast(x, "POINT", warn = FALSE)
  }

  stopifnot(inherits(x, "sfc_POLYGON") | inherits(x, "sfc_POINT"))

  y <- st_sfc(st_polygon(list(
    matrix(c(-180, 0, 0, -180, -180, -90, -90, 90, 90, -90), ncol = 2)
  )), crs = 4326)
  y <- st_transform(y, st_crs(x))
  lon.add <- abs(unname(st_bbox(y))[1] * 2)

  y.x <- suppressMessages(st_intersects(y, x)[[1]])
  y.x.no <- (1:length(x))[-y.x]

  x1 <- st_sfc(x[y.x] + c(lon.add, 0), crs = x.crs)
  x2 <- x[y.x.no]

  preview360.areacheck(x, x1, x2)

  st_sfc(c(x1, x2)[order(c(y.x, y.x.no))], crs = x.crs)
}


###############################################################################
#  See top of file for details
preview360_mod <- function (x) {
  UseMethod("preview360_mod", x)
}

#------------------------------------------------------------------------------
preview360_mod.sf <- function(x) {
  x.crs <- st_crs(x)

  if (st_is_longlat(x)) {
    st_sf(
      st_set_geometry(x, NULL),
      geometry = (st_geometry(x) + c(360, 90)) %% c(360) - c(0, 90),
      crs = x.crs, agr = st_agr(x)
    )

  } else {
    y <- st_sfc(st_polygon(list(
      matrix(c(-180, 0, 0, -180, -180, -90, -90, 90, 90, -90), ncol = 2)
    )), crs = 4326)
    y <- st_transform(y, st_crs(x))
    lon.add <- abs(unname(st_bbox(y))[1] * 2)
    lat.add <- abs(unname(st_bbox(y))[2])

    st_sf(
      st_set_geometry(x, NULL),
      geometry = (st_geometry(x) + c(lon.add, lat.add)) %% c(lon.add) - c(0, lat.add),
      crs = x.crs, agr = st_agr(x)
    )
  }
}

#------------------------------------------------------------------------------
preview360_mod.sfc <- function(x) {
  x.crs <- st_crs(x)

  if (st_is_longlat(x)) {
    st_sfc((x + c(360, 90)) %% c(360) - c(0, 90), crs = x.crs)

  } else {
    y <- st_sfc(st_polygon(list(
      matrix(c(-180, 0, 0, -180, -180, -90, -90, 90, 90, -90), ncol = 2)
    )), crs = 4326)
    y <- st_transform(y, x.crs)
    lon.add <- abs(unname(st_bbox(y))[1] * 2)
    lat.add <- abs(unname(st_bbox(y))[2])

    st_sfc(
      (x + c(lon.add, lat.add)) %% c(lon.add) - c(0, lat.add), crs = x.crs
    )
  }
}


###############################################################################
#  See top of file for details
preview360_split_intersection <- function(x) {
  UseMethod("preview360_split_intersection")
}

#------------------------------------------------------------------------------
preview360_split_intersection.sf <- function(x) {
  x.agr <- st_agr(x)
  x.crs <- st_crs(x)

  # Because we're using st_intersection(), sf geom doesn't have type reqs

  y1 <- st_sfc(st_polygon(list(
    matrix(c(-180, 0, 0, -180, -180, -90, -90, 90, 90, -90), ncol = 2)
  )), crs = 4326)
  y1 <- st_transform(y1, st_crs(x))
  lon.add <- abs(unname(st_bbox(y1))[1] * 2)

  y2 <- st_sfc(st_polygon(list(
    matrix(c(180, 0, 0, 180, 180, -90, -90, 90, 90, -90), ncol = 2)
  )), crs = 4326)
  y2 <- st_transform(y2, st_crs(x))

  x.y1 <- suppressMessages(st_intersection(x, y1))
  x.y2 <- suppressMessages(st_intersection(x, y2))

  x.y1.df <- st_set_geometry(x.y1, NULL)
  x.y1.geom <- st_geometry(x.y1)

  x.y1.360 <- x.y1.df %>%
    st_sf(geometry = x.y1.geom + c(lon.add, 0), crs = x.crs, agr = x.agr)

  preview360.areacheck(x, x.y1.360, x.y2)

  st_set_agr(rbind(x.y2, x.y1.360), x.agr)
}

#------------------------------------------------------------------------------
preview360_split_intersection.sfc <- function(x) {
  x.crs <- st_crs(x)

  # Because we're using st_intersection(), sfc object doesn't have type reqs

  y1 <- st_sfc(st_polygon(list(
    matrix(c(-180, 0, 0, -180, -180, -90, -90, 90, 90, -90), ncol = 2)
  )), crs = 4326)
  y1 <- st_transform(y1, st_crs(x))
  lon.add <- abs(unname(st_bbox(y1))[1] * 2)

  y2 <- st_sfc(st_polygon(list(
    matrix(c(180, 0, 0, 180, 180, -90, -90, 90, 90, -90), ncol = 2)
  )), crs = 4326)
  y2 <- st_transform(y2, st_crs(x))

  x.y1 <- suppressMessages(st_intersection(x, y1))
  x.y2 <- suppressMessages(st_intersection(x, y2))

  x.y1.360 <- st_sfc(x.y1 + c(lon.add, 0), crs = x.crs)

  preview360.areacheck(x, x.y1.360, x.y2)

  st_sfc(c(x.y2, x.y1.360), crs = x.crs)
}

###############################################################################
preview360.areacheck <- function(x, y1, y2) {
  x.area <- as.numeric(sum(st_area(x)))
  y.area <- as.numeric(sum(st_area(y1), st_area(y2)))
  area.diff.perc <- (x.area - y.area) / x.area

  if (area.diff.perc > 0.001) {
    stop("The GUI was unable to convert the polygon ",
         "to the longitude range of [0, 360] decimal degrees")
  }

  TRUE
}

###############################################################################
