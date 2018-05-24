### Code for final processing steps and adding data to vals$models...


### Inputs:
# sf.load.ll       # Model predictions with crs = crs.ll
# sf.load.orig     # Model predictions in original projection
# spdf.pix         # Model predictions as a SpatialPixelsDF object
# pred.type        # Prediction type
# model.res        # Resolution of model grid
# model.name       # File name of model predictions
# data.names       # Names of data columns with predictions, errors, and weights


if (!exists("sf.load.orig")) sf.load.orig <- sf.load.ll
# Check of if sf.load.orig is valid happens in individual functions so that
#   it can happen before transformation to crs.ll

### Set names for sf.load.ll and sf.load.orig, both for data and sfc columns
names(sf.load.ll)   <- c("Pred", "Error", "Weight", "Pixels", "geometry")
names(sf.load.orig) <- c("Pred", "Error", "Weight", "Pixels", "geometry")

attr(sf.load.ll, "sf_column")   <- "geometry"
attr(sf.load.orig, "sf_column") <- "geometry"

st_agr(sf.load.ll)   <- "constant"
st_agr(sf.load.orig) <- "constant"

### Calculate predicted abundance if 'Absolute abundance' is selected
if (pred.type == 1) {
  spdf.abund <- unname(round(model_abundance(sf.load.orig, "Pred"), 0))
} else {
  spdf.abund <- "N/A"
}


### Create vector of specs about the predictions, added to list of vectors
specs.curr <- c(
  model.res, nrow(sf.load.ll), sum(!is.na(sf.load.ll$Pred)), spdf.abund,
  paste0("(", paste(round(st_bbox(sf.load.ll), 0)[1:2], collapse = ", "),
         "), (",
         paste(round(st_bbox(sf.load.ll), 0)[3:4], collapse = ", "), ")")
)


### Save objects to reactiveValues
vals$models.ll <- c(vals$models.ll, list(sf.load.ll))
vals$models.orig <- c(vals$models.orig, list(sf.load.orig))
vals$models.names <- c(vals$models.names, model.name)
vals$models.data.names <- c(vals$models.data.names, data.names)
vals$models.pred.type <- c(vals$models.pred.type, pred.type)
vals$models.specs <- c(vals$models.specs, list(specs.curr))
