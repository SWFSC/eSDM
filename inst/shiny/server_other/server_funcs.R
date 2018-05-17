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

  return(gis.file)
}



#------------------------------------------------------------------------------
# Title
#
# Attempt to make an invalid polygon (poly.invalid) valid
# Perform checks to see if area/predicted abundance were changed much (?)
#
# TODO: What to do if polygon can't be made valid -
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

    alert.text <- paste0(alert1, "\n", alert2, "\n", alert3, "\n", alert4)

    shinyjs::alert(alert.text)

    return(poly.maybe)
  }
}


#------------------------------------------------------------------------------
## Sort by lat and then long; return crs.ll and orig proj version of file
#    Requires that 'gis.loaded' is an sf object

gis_model_check <- function(gis.loaded) {
  validate(
    need(identical(class(gis.loaded), c("sf", "data.frame")),
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
    need(!is.na(st_crs(gis.loaded)),
         "Error: GIS file does not have defined projection")
  )
  if (identical(st_crs(gis.loaded), crs.ll)) {
    list.toreturn <- list(gis.loaded, gis.loaded)
  } else {
    list.toreturn <- list(st_transform(gis.loaded, crs.ll), gis.loaded)
  }

  # Check that extent is as expected
  ext <- st_bbox(list.toreturn[[1]])
  # Run some dateline correction function here..?
  validate(
    need(all(ext["xmax"] <= 180 & ext["xmin"] >= -180),
         "Error: Raster longitude extent is not -180 to 180 degrees"),
    need(all(ext["ymax"] <= 90 & ext["ymin"] >= -90),
         "Error: Raster latitude extent is not -90 to 90 degrees")
  )

  # Return list of sf objects
  return(list.toreturn)
}


#------------------------------------------------------------------------------
# From https://github.com/r-spatial/sf/issues/346
# st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
