###############################################################################
###############################################################################
# Reads in a GIS shapefile from a fileInput ouptput in a Shiny app and
#   returns a sf object
# From \url{https://github.com/leonawicz/nwtapp/blob/master/mod_shpPoly.R}

read.shp.shiny <- function(file.in.list) {
  infiles <- file.in.list$datapath
  dir <- unique(dirname(infiles))
  outfiles <- file.path(dir, file.in.list$name)
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))

  gis.file <- try(st_read(dir, strsplit(file.in.list$name[1], "\\.")[[1]][1],
                          quiet = TRUE),
                  silent = TRUE)

  if (inherits(gis.file, "sf")) {
    gis.file
  } else {
    try(stopifnot(inherits(gis.file, "sf")), silent = TRUE)
  }
}


###############################################################################
###############################################################################
# Attempt to make an invalid polygon (poly.invalid) valid
# Perform checks to see if area/predicted abundance were changed much (?)
#
# TODO: What exactly to do if polygon can't be made valid -
#   Return original poly along with ALERT about invalidity and possible errors if that polygon is used?

make_poly_valid <- function(poly.invalid, dens.col = NA, poly.info = NA, message.invalid = NA) {
  poly.maybe <- lwgeom::st_make_valid(poly.invalid)

  #----------------------------------------------------------------------------
  check1 <- !all(st_is_valid(poly.maybe))
  if (inherits(poly.invalid, "sf")) {
    check2 <- !identical(
      class(st_geometry(poly.maybe)), class(st_geometry(poly.invalid))
    )
  } else {
    check2 <- !identical(class(poly.maybe), class(poly.invalid))
  }

  #----------------------------------------------------------------------------
  if (check1 || check2) {
    alert1 <- ifelse(
      is.na(poly.info), "The polygon currently being processed is invalid.",
      paste("The", pol.info, "polygon is invalid.")
    )
    if (!is.na(message.invalid)) {
      alert1 <- paste(
        alert1, "The error output was:<br>", message.invalid
      )
    }

    alert2 <- paste(
      "The GUI was unable to make the polygon valid using the st_make_valid() function",
      "from the lwgeom package (see the lwgeom package documentation",
      "for more details about this function).",
      "You may attempt to still use this polygon in the GUI, particularly if the",
      "invlaid region will be clipped later, but this is NOT recommended as the",
      "invlaid polygon likely will cause errors in the GUI."
    )

    showModal(modalDialog(
      title = "Important message - imported polygon is invalid and the GUI was unable to fix it",
      HTML(paste0(alert1, "<br><br>", alert2))
    ))

    poly.invalid

    #--------------------------------------------------------------------------
  } else {
    # Get area difference
    area1 <- as.numeric(sum(st_area(poly.invalid)))
    area.dif <- abs(as.numeric(sum(st_area(poly.maybe))) - area1)

    area.dif.char <- sprintf(as.character(round(area.dif / 1e+06, 4)), "%3")
    if (identical(area.dif.char, "0")) area.dif.char <- "0.000"

    area.dif.perc.char <- sprintf(as.character(round((area.dif / area1) * 100, 4)), "%3")
    if (identical(area.dif.perc.char, "0")) area.dif.perc.char <- "0.000"

    # Get predicted abundance difference
    if (!is.na(dens.col)) {
      abund1 <- eSDM::model_abundance(poly.invalid, dens.col)
      abund.dif <- eSDM::model_abundance(poly.maybe, dens.col) - abund1

      abund.dif.char <- sprintf(as.character(round(abund.dif / 1e+06, 4)), "%3")
      if (identical(abund.dif.char, "0")) abund.dif.char <- "0.000"

      abund.dif.perc.char <- sprintf(as.character(round((abund.dif / abund1) * 100, 4)), "%3")
      if (identical(abund.dif.perc.char, "0")) abund.dif.perc.char <- "0.000"
    }


    ###################################
    # Generate alert text to be displayed
    alert1 <- ifelse(
      is.na(poly.info), "The polygon currently being processed was invalid.",
      paste("The", pol.info, "polygon was invalid.")
    )
    if (!anyNA(message.invalid)) {
      alert1 <- paste(
        alert1, "The error output was:<br>",
        paste0(
          "<span style=\"color: red;\">",
          paste(message.invalid, collapse = "; "),
          "</span>"
        )
      )
    }

    alert2 <- paste(
      "The GUI made the polygon valid using the st_make_valid() function",
      "from the lwgeom package (see the lwgeom package documentation",
      "for more details about this function).",
      "You may safely continue using this object in the GUI as long as",
      "you are comfortable with the change in area reported below.",
      "You can use the preview functionality or export this polygon to ensure",
      "that no unexpected changes to the geometry occurred."
    )

    alert3 <- paste(
      "The difference between the area of the valid polygon and",
      "the area of the original polygon is",
      area.dif.char,
      "square km, which is",
      area.dif.perc.char,
      "percent different than the area of the original polygon."
    )

    if (!is.na(dens.col)) {
      alert4 <- paste(
        "The difference between the predicted abundance of the valid SDM",
        "and the predicted abundance of the original SDM is",
        abund.dif.char,
        "animals, which is",
        abund.dif.perc.char,
        "percent different than the predicted abundance of the original SDM."
      )
    } else {
      alert4 <- NULL
    }

    # shinyjs::alert(
    #   paste0(alert1, "\n\n", alert2, "\n\n", alert3, "\n\n", alert4)
    # )

    showModal(modalDialog(
      title = "Important message - polygon was invalid but made valid",
      HTML(paste0(
        alert1, "<br><br>", alert2, "<br><br>", alert3, "<br><br>", alert4
      ))
    ))

    poly.maybe
  }
}


###############################################################################
###############################################################################
# Create sfc object from data frame (from csv) with only long and lat,
#   respectively, as columns. crs set as crs.prov
pts_to_sfc_vertices_shiny <- function(x, crs.prov, progress.detail) {
  obj.sfc <- try(eSDM::pts_to_sfc_vertices(x, crs.prov), silent = TRUE)

  validate(
    need(inherits(obj.sfc, "sfc"),
         paste("Error: The polygon could not be created",
               "from the points in the provided .csv file.",
               "Please ensure that the .csv file has the longitude points",
               "in the first column, the latitude points in the second",
               "column, and that the provided points form a closed",
               "and valid polygon"))
  )

  obj.sfc <- check_dateline(obj.sfc, 60, progress.detail)
  check_valid(obj.sfc, progress.detail)
}


###############################################################################
###############################################################################
# check_ functions: run on sdm sf objects as they're loaded to ensure
#   correct formatting, etc

#------------------------------------------------------------------------------
## Sort by lat and then long; return crs.ll and orig proj version of file
#    Requires that 'x' is an sf object

check_gis_crs <- function(x) {
  validate(
    need(inherits(x, "sf"),
         "Error: GIS object was not read in properly") %then%
      need(st_crs(x)$proj4string,
           "Error: GIS file does not have defined projection")
  )

  if (identical(st_crs(x), crs.ll)) {
    list(x, x)
  } else {
    list(st_transform(x, crs.ll), x)
  }
}


#------------------------------------------------------------------------------
# Adjust sf object from 0 - 360 range to -180 to 180 range and check range
check_dateline <- function(x, wrap.offset = 10, progress.detail = FALSE) {
  stopifnot(
    inherits(x, c("sf", "sfc")),
    inherits(wrap.offset, c("numeric", "integer")),
    inherits(progress.detail, "logical")
  )

  if (progress.detail) {
    on.exit(incProgress(0, detail = ""))
    incProgress(0, detail = "Checking if object spans the dateline")
  }

  x.orig <- x
  x.crs.orig <- st_crs(x.orig)
  dateline.flag <- FALSE
  validate(
    need(x.crs.orig$proj4string,
         "Error: The object does not have a defined coordinate system")
  )

  if (!grepl("proj=longlat", x.crs.orig$proj4string)) {
    x <- st_transform(x, crs.ll)
  }

  if (st_bbox(x)[3] > 180) {
    dateline.flag <- TRUE
    if (progress.detail) {
      incProgress(0, detail = "Object does span the dateline; processing now")
    }
    x <- suppressWarnings(st_wrap_dateline(
      x, c("WRAPDATELINE=YES", paste0("DATELINEOFFSET=", wrap.offset))
    ))
  }

  ext <- st_bbox(x)
  validate(
    need(all(ext["xmax"] <= 180 & ext["xmin"] >= -180),
         paste("Error: The GUI was unable to process this object;",
               "please manually ensure that the longitude range of the",
               "object is [-180, 180] and then re-import the object into the GUI")),
    need(all(ext["ymax"] <= 90 & ext["ymin"] >= -90),
         paste("Error: The GUI was unable to process this object;",
               "please manually ensure that the latitude range of the",
               "object is [-90, 90] and then re-import the object into the GUI"))
  )

  if (dateline.flag){
    st_transform(x, x.crs.orig)
  } else {
    x.orig
  }
}


#------------------------------------------------------------------------------
check_valid <- function(x, progress.detail = FALSE) {
  stopifnot(
    inherits(x, c("sf", "sfc")),
    is.logical(progress.detail)
  )

  if (progress.detail) {
    on.exit(incProgress(0, detail = ""))
    incProgress(0, detail = "Checking if polygons are valid")
  }

  x.valid <- st_is_valid(x, reason = TRUE)

  if (!isTruthy(all(x.valid == "Valid Geometry"))) { #isTruthy() is for NA cases
    if (progress.detail) incProgress(0, detail = "Making polygons valid")
    x.message <- x.valid[x.valid != "Valid Geometry"]

    make_poly_valid(x, message.invalid = x.message)
  } else {
    x
  }
}


#------------------------------------------------------------------------------
check_pred_weight <- function(x, pred.idx, weight.idx, pred.na.idx, weight.na.idx) {
  stopifnot(inherits(pred.idx, c("numeric", "integer")))
  if (inherits(x, "sf")) {
    x.orig <- x
    x <- st_set_geometry(x, NULL)
  }

  if (!inherits(pred.na.idx, "logical")) x[pred.na.idx, pred.idx] <- NA
  if (!inherits(weight.na.idx, "logical")) x[weight.na.idx, weight.idx] <- NA

  validate(
    need(inherits(x[, pred.idx], c("numeric", "integer")),
         paste("Error: Unable to process the prediciton data, please...")),
    if (!is.na(weight.idx)) {
      need(inherits(x[, weight.idx], c("numeric", "integer")),
           paste("Error: Unable to process the weight data, please..."))
    }
  )

  if (exists("x.orig")) {
    st_sf(x, st_geometry(x.orig), agr = "constant")
  } else {
    x
  }
}

###############################################################################
###############################################################################
# Originally internal eSDM functions that aren't used in exported functions

# Calculate break points for density intervals
# Break points are at: 2%, 5%, 10%, 15%, 20%, 25%, 30%, 35%, 40%
breaks_calc <- function(x, breaks = c(seq(0.4, 0.05, by = -0.05), 0.02)) {
  x <- x[!is.na(x)]
  x <- sort(x, decreasing = TRUE)

  c(-Inf, x[ceiling(breaks * length(x))], Inf)
}


# Sort x by col1 and then (if applicable) col2
data_sort <- function(x, col1 = 1, col2 = NA) {
  if (!is.na(col2)) x <- x[order(x[, col2]), ]
  x[order(x[, col1]), ]
}


# Determine whether all values in x are equal;
# From Hadley on stack overflow, simplified version of scales::zero_range()
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}


# Get last n element(s) from string x
# From https://stackoverflow.com/questions/7963898
substr_right <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}


# Determine which elements of the x are one of invalid
# Invalid elements are "N/A", "n/a", "na", "NaN", or ""
na_which <- function(x) {
  na.char <- c("N/A", "n/a", "na", "NaN", "")

  na.idx <- suppressWarnings(
    c(which(is.na(x)), which(is.nan(x)), which(x %in% na.char), which(x < 0))
  )

  if (length(na.idx) == 0) NA else sort(unique(na.idx))
}


# Generate message reporting length of x
# This message was built to refer to prediction values
na_pred_message <- function(x) {
  if (anyNA(x)) {
    "No prediction values were classified as NA"
  } else {
    len.x <- length(x)
    ifelse(len.x == 1,
           paste(len.x, "prediction value was classified as NA"),
           paste(len.x, "prediction values were classified as NA"))
  }
}


# Generate message reporting length of x
# This message was built to refer to weight values, including if any non-NA
#   prediction values corresponded to NA weight values
na_weight_message <- function(x, y) {
  len.x <- length(x)
  if (anyNA(x)) {
    "No weight values were classified as NA"

  } else if (!all(x %in% y)) {
    paste0(
      ifelse(len.x == 1,
             paste(len.x, "weight value was classified as NA"),
             paste(len.x, "weight values were classified as NA")),
      "<br/>Some non-NA prediction values have NA weight values"
    )

  } else {
    paste0(
      ifelse(len.x == 1,
             paste(len.x, "weight value was classified as NA"),
             paste(len.x, "weight values were classified as NA")),
      "<br/>No non-NA prediction values have NA weight values"
    )
  }
}


# # Normalize vector of model predictions, 'x'
# normalize <- function(x) {
#   num <- (x - min(x, na.rm = TRUE))
#   denom <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#
#   num / denom
# }


# Round 'x' to nearest 'base' value
mround <- function(x, base, floor.use = FALSE, ceiling.use = FALSE) {
  if (floor.use) {
    base * floor(x / base)

  } else if (ceiling.use) {
    base * ceiling(x / base)

  } else {
    base * round(x / base)
  }
}

# Capitalize first letter of first element of string
esdm_simple_cap <- function(x, all = FALSE) {
  if (all) {
    s <- strsplit(x, " ")[[1]]
    paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse = " ")
  } else {
    paste0(toupper(substring(x, 1,1)), substring(x, 2), collapse = " ")
  }
}


###############################################################################
###############################################################################
# [0, 360] - related functions

# Adapted from https://github.com/r-spatial/sf/issues/280
# preview360_split(): Converts spatial object to range [0, 360] by splitting
#   polygons that span the dateline and adding 360 to longitude coordinates of
#   polygons in [-180, 0]
#   Main advantage: this method is faster
# preview360_mod(): Converts spatial object to range [0, 360] using the
#   mod ('%%') function, and thus without splitting polygons that span the
#   dateline
#   Main advantage: this method conserves polygons that span the dateline;
#     best for exporting or union-ing single polygons that had been split
#     along the dateline


#------------------------------------------------------------------------------
### Tests if x spans the dateline
check_360 <- function(x) {
  stopifnot(isTruthy(st_crs(x)[[2]]))

  x.bbox.lon <- round(unname(st_bbox(x)), 3)

  identical(abs(x.bbox.lon[1]), x.bbox.lon[3])
}


#------------------------------------------------------------------------------
### Top-level for converting dateline-spanning preds back to 0-360 if nec
check_preview360_split <- function (x) {
  if (check_360(x)) preview360_split(x) else x
}


###############################################################################
#  See above for details
preview360_split <- function(x) {
  UseMethod("preview360_split")
}

#----------------------------------------------------------
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

  stopifnot(as.numeric(sum(st_area(x)) - sum(st_area(x1), st_area(x2))) < 1)

  st_set_agr(rbind(x1, x2)[order(c(y.x, y.x.no)), ], x.agr)
}

#----------------------------------------------------------
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

  stopifnot(as.numeric(sum(st_area(x)) - sum(st_area(x1), st_area(x2))) < 1)

  st_set_crs(c(x1, x2)[order(c(y.x, y.x.no))], x.crs)
}


###############################################################################
#  See above for details
preview360_mod <- function (x) {
  UseMethod("preview360_mod", x)
}

#----------------------------------------------------------
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

#----------------------------------------------------------
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

    st_sfc((x + c(lon.add, lat.add)) %% c(lon.add) - c(0, lat.add), crs = x.crs)
  }
}

###############################################################################
