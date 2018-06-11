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
  purrr::set_names(sf.load.ll, c("Pred", "Weight", "Pixels")) %>%
  st_sf(geometry = st_geometry(sf.load.ll), agr = "constant")
sf.load.orig <- st_set_geometry(sf.load.orig, NULL) %>%
  purrr::set_names(sf.load.orig, c("Pred", "Weight", "Pixels")) %>%
  st_sf(geometry = st_geometry(sf.load.orig), agr = "constant")


### Calculate predicted abundance if 'Absolute abundance' is selected
# if (pred.type == 1) {
#   abund <- unname(round(model_abundance(sf.load.orig, "Pred"), 0))
# } else {
#   abund <- "N/A"
# }


### Create vector of specs about the predictions, added to list of vectors
# Specs are: resolution, cell count, non-NA prediction count, abundance,
#   and lat/long extent
specs.curr <- c(
  model.res, nrow(sf.load.ll), sum(!is.na(sf.load.ll$Pred)),
  ifelse(pred.type == 1,
         unname(round(model_abundance(sf.load.orig, "Pred"), 0)),
         "N/A"),
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
