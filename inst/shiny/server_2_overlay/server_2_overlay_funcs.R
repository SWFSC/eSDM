#' Title
#'
#' Check that provided sf object has a valid crs, and return crs.ll version
#'
#' @export

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
#' Title
#'
#' Attempt to make an invalid polygon (poly.invalid) valid
#'
#' @export

valid.poly <- function(poly.invalid, description = NULL) {
  # print(paste("Validating", description))
  poly.maybe <- suppressWarnings(gBuffer(poly.invalid, byid = TRUE, width = 0))
  # Warnings suppressed for if poly.invalid has crs.ll

  # Is gBuffer method didn't work, then try clgeo_Clean()
  if (!(suppressWarnings(gIsValid(poly.maybe)) |
        (sum(area(poly.maybe)) - sum(area(poly.invalid))) < 1e+06)) {
    # print("Using clgeo_Clean() on polygon", description)
    poly.maybe <- cleangeo::clgeo_Clean(poly.invalid)
  }

  # Is gBuffer method didn't work, then try clgeo_Clean()
  validate(
    need(suppressWarnings(gIsValid(poly.maybe)),
         paste("Error: Could not make polygon", description, "valid")),
    need((sum(area(poly.maybe)) - sum(area(poly.invalid))) < 1e+06,
         paste("Error: While being made valid, the area of the polygon",
               description, "changed by more than 1 square kilometer")
    )
  )

  poly.maybe
}

