# Title
#
# Check that provided sf object has a valid crs, and return crs.ll version
#

overlay.gis.crs <- function(gis.loaded) {
  validate(
    need(class.sf.sfc(gis.loaded, "sfc"),
         "Error: Object passed to overlay.gis.crs() is not an sfc object") %then%
      need(!is.na(st_crs(gis.loaded)),
           "Error: GIS file does not have defined projection")
  )

  sf.ll <- st_transform(gis.loaded, crs.ll)

  validate(
    need(st_bbox(sf.ll)["xmax"] <= 180 & st_bbox(sf.ll)["xmin"] >= -180,
         "Error: Shapefile has longitudes > 180 or < -180 degrees"),
    need(st_bbox(sf.ll)["ymax"] <= 90 & st_bbox(sf.ll)["ymin"] >= -90,
         "Error: Shapefile has latitudes > 90 or < -90 degrees")
  )

  return(sf.ll)
}


#------------------------------------------------------------------------------
# Title
#
# Attempt to make an invalid polygon (poly.invalid) valid
# Perform checks to see if area/predicted abundance were changed much (?)
#

polyValidCheck <- function(poly.invalid, dens.col = NA) {
  poly.maybe <- lwgeom::st_make_valid(poly.invalid)

  if (!all(st_is_valid(poly.maybe))) {
    stop("Could not make polygon valid")

  } else {
    # Check that area wasn't changed much
    area1 <- as.numeric(sum(st_area(poly.maybe)))
    area.dif <- abs(as.numeric(sum(st_area(poly.maybe)) - area2))
    stopifnot(area.dif / area1)#< 1e+06

    # Check that predicted abundance wasn't changed much
    if (!is.na(dens.col)) {
      abund1 <- model.abundance(poly.invalid, dens.col)
      abund.dif <- model.abundance(poly.maybe, dens.col) - abund1

      stopifnot(abund.dif / abund2 < 0.01)
    }

    return(poly.maybe)
  }
}


# From https://github.com/r-spatial/sf/issues/346
# st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))




