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
# incProgress(0.1)


### Set names for sf.load.ll and sf.load.orig, both for data and sfc columns
names(sf.load.ll)[1:3] <- c("Pred", "Weight", "Pixels")
names(sf.load.orig)[1:3] <- c("Pred", "Weight", "Pixels")

st_agr(sf.load.ll) <- "constant"
st_agr(sf.load.orig) <- "constant"


### Calculate predicted abundance if 'Absolute abundance' is selected
# if (pred.type == 1) {
#   abund <- unname(round(model_abundance(sf.load.orig, "Pred"), 0))
# } else {
#   abund <- "N/A"
# }


### Create vector of specs about the predictions, added to list of vectors
### Specs are: resolution, cell count, non-NA prediction count, abundance,
###   and lat/long extent
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
