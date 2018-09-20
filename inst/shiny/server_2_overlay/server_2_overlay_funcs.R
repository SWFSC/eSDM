# Check that provided sf object has a valid crs, and return crs.ll version
overlay_gis_check <- function(gis.loaded) {
  validate(
    need(inherits(gis.loaded, "sfc"),
         "Error: Object passed to overlay.gis.crs() is not an sfc object") %then%
      need(!is.na(st_crs(gis.loaded)),
           "Error: GIS file does not have defined projection")
  )

  sf.ll <- st_transform(gis.loaded, crs.ll)

  if (st_bbox(sf.ll)[3] > 180) sf.ll <- st_wrap_dateline(sf.ll)

  validate(
    need(st_bbox(sf.ll)["xmax"] <= 180 & st_bbox(sf.ll)["xmin"] >= -180,
         "Error: Shapefile has longitudes > 180 or < -180 degrees"),
    need(st_bbox(sf.ll)["ymax"] <= 90 & st_bbox(sf.ll)["ymin"] >= -90,
         "Error: Shapefile has latitudes > 90 or < -90 degrees")
  )

  sf.ll
}
