### Code for reading in/loading predictions from a raster (.tif file)


###############################################################################
### Get indices of predictions that area NA's using na_which()
model_gis_raster_NA_idx_pred <- reactive({
  req(read_model_gis_raster())
  data.raster.pred <- read_model_gis_raster()[[1]]$Pred

  na_which(data.raster.pred)
})


###############################################################################
# Load and process data from raster

### Read in data and return SPolyDFs
read_model_gis_raster <- reactive({
  req(input$model_gis_raster_file)

  ### Ensure file extension is .tif
  if (input$model_gis_raster_file$type != "image/tiff") return()

  withProgress(message = "Loading GIS raster", value = 0.4, {
    gis.file.raster <- try(raster(input$model_gis_raster_file$datapath,
                                  band = input$model_gis_raster_band),
                           silent = TRUE)
    gis.file.success <- inherits(gis.file.raster, "RasterLayer")

    ### If specified file could be loaded as a raster, process raster
    if (gis.file.success) {
      incProgress(0.2)
      sf.load.raster <- st_as_sf(as(gis.file.raster, "SpatialPolygonsDataFrame"))
      stopifnot(ncol(sf.load.raster) == 2)
      incProgress(0.4)

      # sf.load.raster, model_gis_raster_NA_idx_pred()

      # Determine resolution of raster cells
      crs.orig <- st_crs(sf.load.raster)$proj4string
      crs.orig.m  <- grepl("+units=m", crs.orig)
      crs.orig.ll <- grepl("+proj=longlat", crs.orig)

      if (crs.orig.ll & crs.orig.m) {
        model.res <- NA

      } else if (crs.orig.ll) {
        z <- gis.file.raster
        z.1 <- round((z@extent@xmax - z@extent@xmin) / z@ncols, 3)
        z.2 <- round((z@extent@ymax - z@extent@ymin) / z@nrows, 3)
        model.res <- ifelse(z.1 == z.2, paste(z.1, "degrees"), NA)
        rm(z)

      } else if (crs.orig.m) {
        z <- gis.file.raster
        z.1 <- round((z@extent@xmax - z@extent@xmin) / z@ncols / 1e+3, 3)
        z.2 <- round((z@extent@ymax - z@extent@ymin) / z@nrows / 1e+3, 3)
        model.res <- ifelse(z.1 == z.2, paste(z.1, "km"), NA)
        rm(z)

      } else {
        model.res <- NA
      }
    }
  })

  # Return appropriate objects
  if (gis.file.success) {
    list(sf.load.raster, model.res)
  } else {
    NULL
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
create_sf_gis_raster <- eventReactive(input$model_create_gis_raster, {
  sf.load.raster <- read_model_gis_raster()[[1]]

  withProgress(message = "Adding model predictions to app", value = 0.3, {
    # Check that pred and weight data are valid
    sf.load.raster <- check_pred_weight(
      sf.load.raster, 1, NA, model_gis_raster_NA_idx_pred(), NA
    )

    # Check long extent, polygon validity, and create crs.ll version if nec
    sf.load.raster <- check_dateline(sf.load.raster, progress.detail = TRUE)
    incProgress(0.1)
    sf.load.raster <- check_valid(sf.load.raster, progress.detail = TRUE)
    incProgress(0.2)
    sf.list <- gis_model_check(sf.load.raster)
    incProgress(0.3)

    # Prepare for 'local' code
    sf.load.ll   <- sf.list[[1]]
    sf.load.orig <- sf.list[[2]]
    model.res    <- read_model_gis_raster()[[2]]

    sf.load.ll <- sf.load.ll %>% st_set_geometry(NULL) %>%
      dplyr::mutate(as.numeric(NA), 1:nrow(sf.load.ll)) %>%
      st_sf(geometry = st_geometry(sf.load.ll), agr = "constant")
    sf.load.orig <- sf.load.orig %>% st_set_geometry(NULL) %>%
      dplyr::mutate(as.numeric(NA), 1:nrow(sf.load.orig)) %>%
      st_sf(geometry = st_geometry(sf.load.orig), agr = "constant")


    pred.type  <- input$model_gis_raster_pred_type
    model.name <- input$model_gis_raster_file$name
    data.names <- list(c(names(sf.load.ll)[1], NA))

    incProgress(0.1)


    #### Code common to csv, raster, and gis_shp/gis_gdb functions ####
    source(
      file.path("server_1_loadModels", "server_1_loadModels_create_local.R"),
      local = TRUE, echo = FALSE, chdir = TRUE
    )
    ###################################################################
  })

  "Model predictions loaded from raster"
})

###############################################################################
