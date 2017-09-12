### Code for final processing steps and adding data to vals$models...
### Used by code for all 4 file types


### Objects used here are set in individual code blocks for the data types
### Requires: 
# spdf.poly.ll     # Model predictions with crs = crs.ll
# spdf.poly.orig   # Model predictions in original projection
# spdf.pix         # Model predictions as a SpatialPixelsDF object
# pred.type        # Prediction type
# model.res        # Resolution of model grid
# model.name       # File name of model predictions
# data.names       # Names of data columns with predictions, errors, and weights


### Calculate predicted abundance if 'Absolute abundance' is selected
if(pred.type == 1) {
  spdf.abund <- unname(round(model.abundance(spdf.poly.ll, "Pred"), 0))
} else {
  spdf.abund <- "N/A"
}


### Create list of specs about the model predictions
specs.curr <- c(model.res, 
                length(spdf.poly.ll), sum(!is.na(spdf.poly.ll$Pred)), spdf.abund, 
                paste(sapply(round(extent(spdf.poly.ll), 0), 
                             function(i) i), collapse = ", "))
incProgress(0.3) # This code is always within withProgress()


### Save objects to reactiveValues
vals$models.pix <- c(vals$models.pix, spdf.pix)
vals$models.ll <- c(vals$models.ll, spdf.poly.ll)
vals$models.orig <- c(vals$models.orig, spdf.poly.orig)
vals$models.names <- c(vals$models.names, model.name)
vals$models.data.names <- c(vals$models.data.names, data.names)
vals$models.pred.type <- c(vals$models.pred.type, pred.type)
vals$models.specs <- c(vals$models.specs, list(specs.curr))
