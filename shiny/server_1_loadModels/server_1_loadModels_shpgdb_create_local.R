### Code for loading model predicitons from both shp and gdb inputs
### Used in create_spdf_gis_shp() and create_spdf_gis_gdb()

withProgress(message = "Adding model predictions to app", value = 0.3, {
  spdf.poly.ll <- model.list[[1]]
  spdf.poly.orig <- model.list[[2]]
  
  error.idx <- ifelse(error.idx == 1, NA, error.idx - 1)
  weight.idx <- ifelse(weight.idx == 1, NA, weight.idx - 1)
  
  all.idx <- c(pred.idx, error.idx, weight.idx)
  
  spdf.data <- data.frame(Pred = spdf.poly.ll@data[,pred.idx],
                           Error = NA, Weight = NA, 
                           Pixels = seq_along(spdf.poly.ll))
  if(!is.na(error.idx)) spdf.data$Error <- spdf.poly.ll@data[,error.idx]
  if(!is.na(weight.idx)) spdf.data$Weight <- spdf.poly.ll@data[,weight.idx]
  
  spdf.poly.ll@data <- spdf.data
  spdf.poly.orig@data <- spdf.data
  incProgress(0.1)
  
  # Calculate resolution of the model predictions
  model.res <- gis.res.calc(spdf.poly.ll, spdf.poly.orig) # JVR 0.9sec
  incProgress(0.2)
  
  # Create SpatialPixelsDF object
  # spdf.pix <- NA
  # x <- spdf.poly.ll
  # r <- raster(x, nrow = 60, ncol = 60) # up for discussion
  # # rasterize takes the value of polygon that overlaps center of raster cell
  # model.raster <- rasterize(x, r, field = "Pred") # JVR 3.5sec
  # spdf.pix <- as(model.raster, "SpatialPixelsDataFrame")
  # names(spdf.pix) <- "Pred"
  spdf.pix <- gis.rasterize.poly(spdf.poly.ll)
  incProgress(0.2)
  
  data.names <- list(names(model.list[[1]])[all.idx])
  
  ###### Code common to raster and gis_shp/gis_gdb functions ######
  source("server_1_loadModels_create_local.R", 
         local = TRUE, echo = FALSE, chdir = TRUE)
  #################################################################
})