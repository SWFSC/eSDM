## Sort by lat and then long; return crs.ll and orig proj version of file
#    Requires that 'gis.loaded' is an sf object

gis.model.check <- function(gis.loaded) {
  validate(
    need(identical(class(gis.loaded), c("sf", "data.frame")),
         "Error: GIS object was not read in properly")
  )

  # Sort spdf by lat and then long so polygons are ordered bottom up
  coords <- data.frame(idx = 1:nrow(gis.loaded), st_coordinates(st_centroid(gis.loaded)))
  idx.sorted <- data.sort(coords, 3, 2)[, 1] # Lat is primary sort
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


shiny.read.csv <- function(file.in.df) {
  validate(
    need(file.in.df$type %in% c("text/csv", "application/vnd.ms-excel"),
         "Error: Selected file is not a csv file")
  )

  csv.df <- read.csv(file.in.df$datapath, stringsAsFactors = FALSE)

  # TODO This should not be in here
  if (all(csv.df[, 1] > 180, na.rm = TRUE)) {
    csv.df[, 1] <- csv.df[, 1] - 360
  }
  validate(
    need(all(csv.df[, 1] <= 180 & csv.df[, 1] >= -180, na.rm = TRUE),
         paste("Error: The provided .csv file must have longitudes in the",
               "range [-180, 180] and latitudes in the range [-90, 90]"))
  )

  return(list(file.in.df$name, csv.df))
}
