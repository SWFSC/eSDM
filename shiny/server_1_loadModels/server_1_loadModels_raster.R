### Code for reading in/loading predictions from a raster (.tif file)


###############################################################################
### Get indices of predictions that area NA's using na.which()
model_gis_raster_NA_idx <- reactive({
  req(read_model_gis_raster())
  data.raster.pred <- read_model_gis_raster()[[1]]$Pred
  
  na.which(data.raster.pred)
})


###############################################################################
# Load and process data from raster

### Read in data and return SPolyDFs
read_model_gis_raster <- reactive({
  req(input$model_gis_raster_file)
  
  # Ensure file extension is .tif
  if(input$model_gis_raster_file$type != "image/tiff") return()
  
  withProgress(message = "Loading GIS raster", value = 0.3, {
    gis.file.raster <- try(raster(input$model_gis_raster_file$datapath, 
                                  band = input$model_gis_raster_band), 
                           silent = TRUE)
    gis.file.success <- ifelse(isTruthy(gis.file.raster), FALSE, TRUE)
    
    # If specified file could be loaded as a raster, process raster
    if(gis.file.success) {
      model.pix <- as(gis.file.raster, "SpatialPixelsDataFrame")
      names(model.pix) <- "Pred"
      # Leave orig data name here
      gis.file.spdf <- as(gis.file.raster, "SpatialPolygonsDataFrame")
      names(gis.file.spdf) <- "Pred"
      incProgress(0.2)
      
      gis.spdfs <- gis.model.check(gis.file.spdf)
      incProgress(0.3)
      
      spdf.ll <- gis.spdfs[[1]]
      ext <- extent(spdf.ll)
      
      # Run dateline correction function here..?
      validate(
        need(all(ext@xmax <= 180 & ext@xmin >= -180), 
             "Error: Raster longitude extent is not -180 to 180 degrees")
      )
      incProgress(0.1)
    }
  })

  if(!gis.file.success) {
    NULL
  } else {
    c(gis.spdfs, model.pix)
  }
})

### Flag for if the raster was fully loaded and processed
output$read_model_gis_raster_flag <- reactive({
  if(is.null(read_model_gis_raster())) FALSE
  else TRUE
})
outputOptions(output, "read_model_gis_raster_flag", suspendWhenHidden = FALSE)

#######################################
### Process data and add it to vals
create_spdf_gis_raster <- eventReactive(input$model_create_gis_raster, {
  model.list <- read_model_gis_raster()
  withProgress(message = "Adding model predictions to app", value = 0.3, {
    spdf.poly.ll <- model.list[[1]]
    spdf.poly.orig <- model.list[[2]]
    spdf.pix <- model.list[[3]]
    
    spdf.data <- data.frame(Pred = spdf.poly.ll$Pred,
                            Error = NA, Weight = NA, 
                            Pixels = seq_along(spdf.poly.ll))
    
    spdf.poly.ll@data <- spdf.data
    spdf.poly.orig@data <- spdf.data
    incProgress(0.3)
    
    # Calculate resolution of the model predictions
    pix.res <- round(unname(spdf.pix@grid@cellsize), 3)
    if(pix.res[1] != pix.res[2]) warning("X and Y pixel width is not the same")
    
    if(grepl("longlat", crs(spdf.pix))) {
      model.res <- paste(pix.res[1], "degrees")
    } else {
      model.res <- paste(round(pix.res[1]/1000, 3), "km")
    }
    incProgress(0.2)
    
    # If raster is not crs.ll, generate crs.ll raster
    if(!identical(crs.ll, crs(spdf.pix))) {
      spdf.pix <- gis.rasterize.poly(spdf.poly.ll)
    }
    
    # Prepare for 'local' code
    pred.type <- input$model_gis_raster_pred_type
    model.name <- input$model_gis_raster_file$name
    data.names <- list(c(names(spdf.pix), NA, NA))
    
    incProgress(0.1)
    
    #### Code common to csv, raster, and gis_shp/gis_gdb functions ####
    source(file.path("server_1_loadModels", 
                     "server_1_loadModels_create_local.R"), 
           local = TRUE, echo = FALSE, chdir = TRUE)
    ###################################################################
  })
  
  return("Model predictions loaded from raster")
})

###############################################################################
