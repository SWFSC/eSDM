# Check that provided sf object has a valid crs, and return crs.ll version
overlay_gis_check <- function(gis.loaded) {
  validate(
    need(inherits(gis.loaded, "sfc"),
         "Error: Error in import, please report this as an issue") %then%
      need(!is.na(st_crs(gis.loaded)$proj4string),
           "Error: The provided object does not have a defined coordinate system")
  )

  sf.ll <- st_transform(gis.loaded, crs.ll)

  sf.ll <- check_dateline(sf.ll)
  check_valid(sf.ll)
}
