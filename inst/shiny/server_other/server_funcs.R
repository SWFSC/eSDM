#-----------------------------------------------------------------------------
# @title Read GIS shpaefile from Shiny fileInput
# @description Read in a GIS shapefile from a fileInput ouptput in a Shiny app
#
# @param file.in.list The list returned by shiny::fileInput()
#
# @return A sf object
#
# @source \url{https://github.com/leonawicz/nwtapp/blob/master/mod_shpPoly.R}
#
# @export

read.shp.shiny <- function(file.in.list) {
  infiles <- file.in.list$datapath
  dir <- unique(dirname(infiles))
  outfiles <- file.path(dir, file.in.list$name)
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))

  gis.file <- try(st_read(dir, strsplit(file.in.list$name[1], "\\.")[[1]][1],
                          quiet = TRUE),
                  silent = TRUE)

  gis.file
}


#------------------------------------------------------------------------------
# Title
#
# Attempt to make an invalid polygon (poly.invalid) valid
# Perform checks to see if area/predicted abundance were changed much (?)
#
# TODO: What exactly to do if polygon can't be made valid -
#   REturn original poly along with ALERT about invalidity and possible errors if that polygon is used?

poly_valid_check <- function(poly.invalid, dens.col = NA, poly.info = NA) {
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

    return(poly.maybe)
  }
}


#------------------------------------------------------------------------------
## Sort by lat and then long; return crs.ll and orig proj version of file
#    Requires that 'gis.loaded' is an sf object

gis_model_check <- function(gis.loaded) {
  validate(
    need(inherits(gis.loaded, "sf"),
         "Error: GIS object was not read in properly")
  )

  # Sort sf object by lat and then long so polygons are ordered bottom up
  coords <- data.frame(
    idx = 1:nrow(gis.loaded),
    st_coordinates(suppressWarnings(st_centroid(gis.loaded)))
  )
  idx.sorted <- data_sort(coords, 3, 2)$idx # Lat is primary sort
  gis.loaded <- gis.loaded[idx.sorted, ]

  # Check crs arguments and project to crs.ll if necessary
  validate(
    need(!is.na(st_crs(gis.loaded)$proj4string),
         "Error: GIS file does not have defined projection")
  )
  if (identical(st_crs(gis.loaded), crs.ll)) {
    list.toreturn <- list(gis.loaded, gis.loaded)
  } else {
    list.toreturn <- list(st_transform(gis.loaded, crs.ll), gis.loaded)
  }

  # Check that extent is as expected
  ext <- st_bbox(list.toreturn[[1]])
  validate(
    need(all(ext["xmax"] <= 180 & ext["xmin"] >= -180),
         "Error: GIS object longitude extent is not -180 to 180 degrees"),
    need(all(ext["ymax"] <= 90 & ext["ymin"] >= -90),
         "Error: GIS object latitude extent is not -90 to 90 degrees")
  )

  list.toreturn
}


#------------------------------------------------------------------------------
# From https://github.com/r-spatial/sf/issues/346
# st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))


#------------------------------------------------------------------------------
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

  return(obj.sfc)
}


#------------------------------------------------------------------------------
# Adjust sf object from 0 - 360 range to -180 to 180 range
check_dateline <- function(x, wrap.offset = 10, progress.detail = FALSE) {
  stopifnot(
    inherits(x, "sf") | inherits(x, "sfc"),
    inherits(wrap.offset, "numeric")
  )

  if (progress.detail) {
    on.exit(incProgress(0, detail = ""))
    incProgress(0, detail = "Checking if SDM spans the dateline")
  }

  x.crs.orig <- st_crs(x)
  if (is.na(x.crs.orig$proj4string))
    stop("Error: SDM does not have a defined coordinate system")

  if (!grepl("proj=longlat", x.crs.orig$proj4string)) {
    x <- st_transform(x, crs.ll)
  }

  if (st_bbox(x)[3] > 180) {
    incProgress(0, detail = "SDM does span the dateline; processing now")
    x <- st_wrap_dateline(
      x,
      options = c("WRAPDATELINE=YES", paste0("DATELINEOFFSET=", wrap.offset))
    )

    if (st_bbox(x)[3] > 180) {
      validate(
        need(FALSE,
             paste("Error: Unable to correct SDM polygon longitude range;",
                   "please ensure that the longitude range of the SDM is",
                   "[-180, 180] and then reload the SDM into the eSDM"))
      )
    }
  }

  if (!identical(st_crs(x), x.crs.orig)) x <- st_transform(x, x.crs.orig)

  x
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
    poly_valid_check(x)

  } else {
    x
  }
}
