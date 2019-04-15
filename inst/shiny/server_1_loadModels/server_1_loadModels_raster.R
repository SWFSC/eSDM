### Code for importing predictions from a raster (.tif file)


###############################################################################
### Get indices of predictions that area NA's using na_which()
model_gis_raster_NA_idx_pred <- reactive({
  na_which(req(read_model_gis_raster())[[1]]$Pred)
})


###############################################################################
# Load and process data from raster

### Read raster data and start processing
read_model_gis_raster <- reactive({
  file.in <- req(input$model_gis_raster_file)

  ### Ensure file extension is .tif or .img (recognized as "")
  if (!(file.in$type %in% c("image/tiff", ""))) return()

  ### Read raster and start processing
  withProgress(message = "Uploading raster", value = 0.4, {
    gis.file.raster <- try(
      raster(file.in$datapath, band = input$model_gis_raster_band),
      silent = TRUE
    )
    gis.file.success <- inherits(gis.file.raster, "RasterLayer")
    incProgress(0.3)

    if (gis.file.success) {
      sf.load.raster <- st_as_sf(
        as(gis.file.raster, "SpatialPolygonsDataFrame")
      )
      stopifnot(ncol(sf.load.raster) == 2)
    }
    incProgress(0.3)
  })

  ### Return appropriate objects
  if (gis.file.success) {
    list(sf.load.raster, gis.file.raster)
  } else {
    NULL
  }
})


### Flag for if the raster was properly read and processed
output$read_model_gis_raster_flag <- reactive({
  if (is.null(read_model_gis_raster())) FALSE else TRUE
})
outputOptions(output, "read_model_gis_raster_flag", suspendWhenHidden = FALSE)


#######################################
### Process data and add it to vals
create_sf_gis_raster <- eventReactive(input$model_create_gis_raster, {
  sf.load.raster <- read_model_gis_raster()[[1]]

  withProgress(message = "Importing predictions from raster", value = 0.2, {
    # Check that pred and weight data are valid
    sf.load.raster <- check_pred_var_weight(
      sf.load.raster, 1, NA, NA, model_gis_raster_NA_idx_pred(), NA, NA
    )

    # Check long extent, polygon validity, and create crs.ll version if nec
    sf.load.raster <- check_dateline(sf.load.raster, progress.detail = TRUE)
    incProgress(0.2)
    sf.load.raster <- check_valid(sf.load.raster, progress.detail = TRUE)
    incProgress(0.2)
    sf.list <- check_gis_crs(sf.load.raster)
    incProgress(0.2)

    # Determine resolution of raster cells
    crs.orig <- st_crs(sf.load.raster)$proj4string
    crs.orig.m  <- grepl("+units=m", crs.orig)
    crs.orig.ll <- st_is_longlat(crs.orig)

    if (crs.orig.ll & crs.orig.m) {
      model.res <- NA

    } else if (crs.orig.ll) {
      z <- read_model_gis_raster()[[2]]
      z.1 <- round((z@extent@xmax - z@extent@xmin) / z@ncols, 3)
      z.2 <- round((z@extent@ymax - z@extent@ymin) / z@nrows, 3)
      model.res <- ifelse(z.1 == z.2, paste(z.1, "degrees"), NA)
      rm(z, z.1, z.2)

    } else if (crs.orig.m) {
      z <- read_model_gis_raster()[[2]]
      z.1 <- round((z@extent@xmax - z@extent@xmin) / z@ncols / 1e+3, 3)
      z.2 <- round((z@extent@ymax - z@extent@ymin) / z@nrows / 1e+3, 3)
      model.res <- ifelse(z.1 == z.2, paste(z.1, "km"), NA)
      rm(z, z.1, z.2)

    } else {
      model.res <- NA
    }
    incProgress(0.1)

    # Prepare for 'local' code
    sf.load.llo   <- sf.list[[1]]
    sf.load.origo <- sf.list[[2]]

    sf.load.ll <- sf.list[[1]] %>%
      st_set_geometry(NULL) %>%
      dplyr::mutate(SE = as.numeric(NA), Weight = as.numeric(NA),
                    idx = 1:nrow(sf.list[[1]])) %>%
      st_sf(geometry = st_geometry(sf.list[[1]]), agr = "constant")
    sf.load.orig <- sf.list[[2]] %>%
      st_set_geometry(NULL) %>%
      dplyr::mutate(SE = as.numeric(NA), Weight = as.numeric(NA),
                    idx = 1:nrow(sf.list[[1]])) %>%
      st_sf(geometry = st_geometry(sf.list[[2]]), agr = "constant")

    data.names <- list(c(names(sf.load.ll)[1], NA, NA))
    pred.type  <- input$model_gis_raster_pred_type
    var.type   <- 2
    model.name <- input$model_gis_raster_file$name
    incProgress(0.1)


    ###### Code common to all importing functions ######
    source(
      file.path("server_1_loadModels", "server_1_loadModels_create_local.R"),
      local = TRUE, echo = FALSE, chdir = TRUE
    )
    ####################################################
  })

  "Predictions imported from raster"
})

###############################################################################
