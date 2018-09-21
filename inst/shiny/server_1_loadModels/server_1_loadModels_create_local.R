### Code for final processing steps and adding data to vals$models...


### Inputs: objects that must be defined before this code chunk is called
# sf.load.ll       # Model predictions with crs = crs.ll
# sf.load.orig     # Model predictions in original projection
# spdf.pix         # Model predictions as a SpatialPixelsDF object
# pred.type        # Prediction type
# model.res        # Resolution of model grid
# model.name       # File name of model predictions
# data.names       # Names of data columns with predictions and weights



if (!exists("sf.load.orig")) sf.load.orig <- sf.load.ll


validate(
  need(inherits(sf.load.ll, "sf") & inherits(sf.load.orig, "sf"),
       "Error 1 in create_local prep")
)
validate(
  need(ncol(sf.load.ll) == 4 & ncol(sf.load.orig) == 4,
       "Error 2 in create_local prep")
)


### Set names and agr for sf.load.ll and sf.load.orig
sf.load.ll <- st_set_geometry(sf.load.ll, NULL) %>%
  purrr::set_names(c("Pred", "Weight", "Pixels")) %>%
  st_sf(geometry = st_geometry(sf.load.ll), agr = "constant")
sf.load.orig <- st_set_geometry(sf.load.orig, NULL) %>%
  purrr::set_names(c("Pred", "Weight", "Pixels")) %>%
  st_sf(geometry = st_geometry(sf.load.orig), agr = "constant")


### Process prediction values based on prediction type
if (pred.type == 1) {
  abund <- unname(round(eSDM::model_abundance(sf.load.orig, "Pred"), 0))

} else if (pred.type == 2) {
  abund <- "N/A"

} else if (pred.type == 3) {
  abund <- sum(sf.load.orig$Pred)

  sdm.area <- st_area(sf.load.orig)
  validate(
    need(all(units(sdm.area)$numerator == c("m", "m")),
         paste("Error: The GUI could not properly calculate the area of",
               "the prediction polygons; please ensure the predictions",
               "are properly formatted and have a defined coordinate system"))
  )

  sdm.area <- as.numeric(st_area(sf.load.orig)) / 1e+06
  sf.load.ll$Pred <- sf.load.ll$Pred / sdm.area
  sf.load.orig$Pred <- sf.load.orig$Pred / sdm.area

  sf.load.ll <- st_set_agr(sf.load.ll, "constant")
  sf.load.orig <- st_set_agr(sf.load.orig, "constant")
}


### Create vector of specs about the predictions, added to list of vectors
# Specs are: resolution, cell count, non-NA prediction count, abundance,
#   and lat/long extent
specs.curr <- c(
  model.res, nrow(sf.load.ll), sum(!is.na(sf.load.ll$Pred)), abund,
  paste0("(", paste(round(st_bbox(sf.load.ll), 0)[c(1, 3)], collapse = ", "),
         "), (",
         paste(round(st_bbox(sf.load.ll), 0)[c(2, 4)], collapse = ", "), ")")
)


### Save objects to reactiveValues
vals$models.ll         <- c(vals$models.ll, list(sf.load.ll))
vals$models.orig       <- c(vals$models.orig, list(sf.load.orig))
vals$models.names      <- c(vals$models.names, model.name)
vals$models.data.names <- c(vals$models.data.names, data.names)
vals$models.pred.type  <- c(vals$models.pred.type, pred.type)
vals$models.specs      <- c(vals$models.specs, list(specs.curr))
