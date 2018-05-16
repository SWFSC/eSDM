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
  if (input$model_gis_raster_file$type != "image/tiff") return()

  withProgress(message = "Loading GIS raster", value = 0.3, {
    gis.file.raster <- try(raster(input$model_gis_raster_file$datapath,
                                  band = input$model_gis_raster_band),
                           silent = TRUE)
    gis.file.success <- isTruthy(gis.file.raster)

    # If specified file could be loaded as a raster, process raster
    if (gis.file.success) {
      incProgress(0.2)
      sf.load.raster <- st_as_sf(as(gis.file.raster, "SpatialPolygonsDataFrame"))
      st_agr(sf.load.raster) <- "constant"
      stopifnot(ncol(sf.load.raster) == 2)
      incProgress(0.1)

      # Determine resolution of raster cells
      z <- gis.file.raster
      z.1 <- round((z@extent@xmax - z@extent@xmin) / z@ncols / 1e+6, 3)
      z.2 <- round((z@extent@ymax - z@extent@ymin) / z@nrows / 1e+6, 3)
      model.res <- ifelse(z.1 == z.2, z.1, NA)

      # QA/QC, ensure that sf.load.orig is valid, and if nec create crs.ll projection
      if (st_bbox(sf.load.raster)[3] > 180) {
        incProgress(0.05, detail = "Polygon(s) span dateline; handling now")
        sf.load.raster <- st_wrap_dateline(sf.load.raster)
      }

      incProgress(detail = "Checking if model polygons are valid")
      if (!all(st_is_valid(sf.load.raster))) {
        incProgress(detail = "Making model polygons valid")
        sf.load.raster <- polyValidCheck(sf.load.raster)
      }
      sf.list <- gis.model.check(sf.load.raster)
      incProgress(0.2, detail = "")
    }
  })

  # Return appropriate objects
  if (!gis.file.success) {
    NULL
  } else {
    c(sf.list, model.res)
  }
})


### Flag for if the raster was fully loaded and processed
output$read_model_gis_raster_flag <- reactive({
  if (is.null(read_model_gis_raster())) FALSE
  else TRUE
})
outputOptions(output, "read_model_gis_raster_flag", suspendWhenHidden = FALSE)


#######################################
### Process data and add it to vals
create_spdf_gis_raster <- eventReactive(input$model_create_gis_raster, {
  data.list <- read_model_gis_raster()
  withProgress(message = "Adding model predictions to app", value = 0.3, {
    sf.load.ll   <- data.list[[1]]
    sf.load.orig <- data.list[[2]]
    model.res    <- data.list[[3]]

    sf.load.ll <- sf.load.ll %>% mutate(Error = NA, Weight = NA,
                                        Pixels = 1:nrow(sf.load.ll))
    sf.load.orig <- sf.load.orig %>% mutate(Error = NA, Weight = NA,
                                            Pixels = 1:nrow(sf.load.orig))

    incProgress(0.3)

    # Calculate resolution of the model predictions
    ### TODO test this more ###
    if (grepl("longlat", st_crs(sf.load.orig)$proj4string)) {
      model.res <- paste(pix.res[1], "degrees")
    } else {
      model.res <- paste(model.res, "km")
    }

    # Assumes raster cells are square
    # y <- table(round(st_area(s)))
    # pix.res <- names(table(round(y, 0)))
    # if (pix.res[1] != pix.res[2]) warning("X and Y pixel width is not the same")
    incProgress(0.2)

    # # If raster is not crs.ll, generate crs.ll raster
    # if (!identical(crs.ll, crs(spdf.pix))) {
    #   spdf.pix <- gis.rasterize.poly(spdf.poly.ll)
    # }

    # Prepare for 'local' code
    pred.type <- input$model_gis_raster_pred_type
    model.name <- input$model_gis_raster_file$name
    data.names <- list(c(names(sf.load.ll)[1], NA, NA))

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
