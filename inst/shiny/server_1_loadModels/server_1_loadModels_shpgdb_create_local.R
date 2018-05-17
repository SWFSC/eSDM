### Code for loading model predicitons from both shp and gdb inputs
### Used in create_spdf_gis_shp() and create_spdf_gis_gdb()

withProgress(message = "Adding model predictions to app", value = 0.3, {
  sf.load.ll.1   <- sf.list[[1]]
  sf.load.orig.1 <- sf.list[[2]]

  error.idx <- NA #ifelse(error.idx == 1, NA, error.idx - 1)
  weight.idx <- ifelse(weight.idx == 1, NA, weight.idx - 1)

  # browser()
  toadd.e <- toadd.w <- NA
  # if(!is.na(error.idx))  {
  #   toadd.e  <- st_set_geometry(sf.load.ll.1, NULL)[, error.idx]
  # }
  if(!is.na(weight.idx)) {
    toadd.w <- st_set_geometry(sf.load.ll.1, NULL)[, weight.idx]
  }

  # Can't pipe to select() because select() can't handle sf objects (yet)
  # Names of sf objects set in load.val.set()
  sf.load.ll <- sf.load.ll.1[, pred.idx] %>%
     mutate(toadd.e, toadd.w, 1:nrow(sf.load.ll.1))
  sf.load.orig <- sf.load.orig.1[, pred.idx] %>%
    mutate(toadd.e, toadd.w, 1:nrow(sf.load.orig.1))
  incProgress(0.3)

  # Calculate resolution of the model predictions
  model.res <- gis_res_calc(sf.load.ll, sf.load.orig) # JVR 0.9sec
  incProgress(0.2)

  data.names <- list(names(sf.list[[1]])[c(pred.idx, error.idx, weight.idx)])


  ###### Code common to raster and gis_shp/gis_gdb functions ######
  source("server_1_loadModels_create_local.R",
         local = TRUE, echo = FALSE, chdir = TRUE)
  #################################################################
})
