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

make_poly_valid <- function(poly.invalid, dens.col = NA, poly.info = NA) {
  poly.maybe <- lwgeom::st_make_valid(poly.invalid)

  if (!all(st_is_valid(poly.maybe))) {
    stop("Could not make polygon valid")

  } else {
    # Check that area wasn't changed much
    area1 <- as.numeric(sum(st_area(poly.maybe)))
    area.dif <- abs(as.numeric(sum(st_area(poly.maybe))) - area1)
    stopifnot((area.dif / area1) < 0.01)

    # Check that predicted abundance wasn't changed much
    if (!is.na(dens.col)) {
      abund1 <- model.abundance(poly.invalid, dens.col)
      abund.dif <- model.abundance(poly.maybe, dens.col) - abund1
      stopifnot((abund.dif / abund1) < 0.01)
    }


    ###################################
    # Generate alert text to be displayed
    alert1 <- ifelse(is.na(poly.info),
                     "The polygon currently being processed was invalid.",
                     paste("The", pol.info, "polygon was invalid."))
    alert2 <- paste(
      "The eSDM made the polygon valid using the st_make_valid() function",
      "from the lwgeom package. Read more about this function here..."
    )
    alert3 <- paste(
      "The difference between the area of the valid polygon and",
      "the area of the original polygon is",
      round(area.dif / 1e+06, 2), "square km, which is",
      round((area.dif / area1) * 100, 2),
      "percent different than the area of the original polygon."
    )
    if (!is.na(dens.col)) {
      alert4 <- paste(
        "The difference between the predicted abundance of the valid sdm",
        "and the predicted abundance of the original sdm is",
        round(abund.dif, 0), "animals, which is",
        round((abund.dif / abund1) * 100, 2),
        "percent different than the predicted abundance of the original sdm."
      )
    } else {
      alert4 <- NULL
    }

    shinyjs::alert(
      paste0(alert1, "\n\n", alert2, "\n\n", alert3, "\n\n", alert4)
    )

    poly.maybe
  }
}


###############################################################################
###############################################################################
# Create sfc object from data frame (from csv) with only long and lat,
#   respectively, as columns. crs set as crs.prov
# Designed only for use within eSDM with sf and dplyr loaded
create_sfc_csv_func <- function(x, crs.prov) {
  stopifnot(
    inherits(x, "data.frame"),
    ncol(x) == 2
  )

  names(x) <- c("lon", "lat")
  if (anyNA(x$lon)) {
    obj.list <- try(
      x %>%
        mutate(na_sum = cumsum(is.na(lon) & is.na(lat))) %>%
        filter(!is.na(lon) & !is.na(lat)) %>%
        group_by(na_sum) %>%
        summarise(list(st_polygon(list(matrix(c(.data$lon, .data$lat), ncol = 2))))),
      silent = TRUE)
    obj.sfc <- try(st_sfc(do.call(rbind, obj.list[, 2])), silent = TRUE)

  } else {
    obj.list <- list()
    obj.sfc <- try(st_sfc(st_polygon(list(as.matrix(x)))),
                   silent = TRUE)
  }

  validate(
    need(isTruthy(obj.list) & inherits(obj.sfc, "sfc"),
         paste("Error: The polygon could not be created",
               "from the provided points.",
               "Please ensure that the .csv file has the longitude points",
               "in the first column, the latitude points in the second",
               "column, and that the provided points form a closed",
               "and valid polygon")) %then%
      need(isTruthy(all(st_is_valid(obj.sfc))), #isTruthy() is for NA cases
           paste("Error: The provided polygon is invalid;",
                 "please ensure that the provided points form a closed",
                 "and valid polygon (no self-intersections)"))
  )
  st_crs(obj.sfc) <- crs.prov

  check_dateline(obj.sfc, 60)

  obj.sfc
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

  # # Sort sf object by lat and then long so polygons are ordered bottom up
  # coords <- data.frame(
  #   idx = 1:nrow(x),
  #   st_coordinates(suppressWarnings(st_centroid(x)))
  # )
  # idx.sorted <- data_sort(coords, 3, 2)$idx # Lat is primary sort
  # x <- x[idx.sorted, ]

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
    incProgress(0, detail = "Checking if SDM spans the dateline")
  }

  x.crs.orig <- st_crs(x)
  validate(
    need(x.crs.orig$proj4string,
         "Error: The SDM does not have a defined coordinate system")
  )

  if (!grepl("proj=longlat", x.crs.orig$proj4string)) {
    x <- st_transform(x, crs.ll)
  }

  if (st_bbox(x)[3] > 180) {
    incProgress(0, detail = "SDM does span the dateline; processing now")
    x <- st_wrap_dateline(
      x, c("WRAPDATELINE=YES", paste0("DATELINEOFFSET=", wrap.offset))
    )

    if (st_bbox(x)[3] > 180) {
      validate(
        need(FALSE,
             paste("Error: Unable to correct SDM polygon longitude range;",
                   "please manually ensure that the longitude range of the",
                   "SDM is [-180, 180] and then reload the SDM into the eSDM"))
      )
    }
  }

  ext <- st_bbox(x)
  validate(
    need(all(ext["xmax"] <= 180 & ext["xmin"] >= -180),
         paste("Error: The eSDM was unable to process this SDM;",
               "please manually ensure that the longitude range of the",
               "SDM is [-180, 180] and then reload the SDM into the eSDM")),
    need(all(ext["ymax"] <= 90 & ext["ymin"] >= -90),
         paste("Error: The eSDM was unable to process this SDM;",
               "please manually ensure that the latitude range of the",
               "SDM is [-90, 90] and then reload the SDM into the eSDM"))
  )

  if (!identical(st_crs(x), x.crs.orig)) {
    st_transform(x, x.crs.orig)
  } else {
    x
  }
}


#------------------------------------------------------------------------------
check_valid <- function(x, progress.detail = FALSE) {
  stopifnot(
    inherits(x, "sf") | inherits(x, "sfc"),
    is.logical(progress.detail)
  )

  if (progress.detail) {
    on.exit(incProgress(0, detail = ""))
    incProgress(0, detail = "Checking if polygons are valid")
  }

  if (!isTruthy(all(st_is_valid(x)))) { #isTruthy() is for NA cases
    if (progress.detail) incProgress(0, detail = "Making polygons valid")
    make_poly_valid(x)
  } else {
    x
  }
}


#------------------------------------------------------------------------------
# TODO: change to S3 method?
check_pred_weight <- function(x, pred.idx, weight.idx, pred.na.idx,
                              weight.na.idx) {
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
