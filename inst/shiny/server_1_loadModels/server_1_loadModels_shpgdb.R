### Code for reading in and processing model predictions from .shp or .gdb


###############################################################################
# gis.model.check() and gis.res.calc() are in ensLoadModels_funcs.R


###############################################################################
# Reactive functions for renderUIs

### Get names of data columns
shp_names_choice_input <- reactive({
  req(read_model_gis_shp())
  choice.input.names <- head(names(read_model_gis_shp()[[1]]), -1)
  choice.input <- (1:length(choice.input.names))
  names(choice.input) <- choice.input.names

  choice.input
})

gdb_names_choice_input <- reactive({
  choice.input.names <- head(names(read_model_gis_gdb()[[1]]), -1)
  choice.input <- (1:length(choice.input.names))
  names(choice.input) <- choice.input.names

  choice.input
})

### Identify NA predictions
model_gis_shp_NA_idx <- reactive({
  req(input$model_gis_shp_names_pred)
  data.shp <- st_set_geometry(read_model_gis_shp()[[1]], NULL)

  data.shp.pred <- data.shp[, as.numeric(input$model_gis_shp_names_pred)]

  na_which(data.shp.pred)
})

model_gis_gdb_NA_idx <- reactive({
  req(input$model_gis_gdb_names_pred)
  data.gdb <- st_set_geometry(read_model_gis_gdb()[[1]], NULL)

  data.gdb.pred <- data.gdb[, as.numeric(input$model_gis_gdb_names_pred)]

  na_which(data.gdb.pred)
})


###############################################################################
# Load and process data from shapefile

### Read in data and return sf object
read_model_gis_shp <- reactive({
  req(input$model_gis_shp_files)

  withProgress(message = "Loading GIS shapefile", value = 0.3, {
    gis.file.shp <- read.shp.shiny(input$model_gis_shp_files)
    incProgress(0.4)

    gis.file.success <- isTruthy(gis.file.shp)
    if (gis.file.success) {
      if (st_bbox(gis.file.shp)[3] > 180) {
        incProgress(0.05, detail = "Polygon(s) span dateline; handling now")
        gis.file.shp <- st_wrap_dateline(gis.file.shp)
      }

      incProgress(detail = "Checking if model polygons are valid")
      if (!all(st_is_valid(gis.file.shp))) {
        incProgress(0.1, detail = "Making model polygons valid")
        gis.file.shp <- poly_valid_check(gis.file.shp)
      }
      sf.list <- gis_model_check(gis.file.shp)
    }
    incProgress(0.15, detail = "")
  })

  if(!gis.file.success) {
    NULL
  } else {
    sf.list
  }
})

### Flag for if the shapefile was fully loaded and processed
output$read_model_gis_shp_flag <- reactive({
  !is.null(read_model_gis_shp())
})
outputOptions(output, "read_model_gis_shp_flag", suspendWhenHidden = FALSE)


#######################################
### Process data and add it to list.all
create_sf_gis_shp <- eventReactive(input$model_create_gis_shp, {
  sf.list <- read_model_gis_shp()

  pred.idx <- as.numeric(input$model_gis_shp_names_pred)
  error.idx <- NA #as.numeric(input$model_gis_shp_names_error)
  weight.idx <- as.numeric(input$model_gis_shp_names_weight)

  model.name <- strsplit(input$model_gis_shp_files$name[1], "\\.")[[1]][1]
  pred.type <- input$model_gis_shp_pred_type

  #### The code from this file is the same as in create_spdf_gis_gdb() ####
  source(file.path("server_1_loadModels",
                   "server_1_loadModels_shpgdb_create_local.R"),
         local = TRUE, echo = FALSE, chdir = TRUE)

  "Model predictions loaded from GIS shapefile"
})


###############################################################################
# Load and process data from file geodatabase (.gdb)

### Read in data and return sf object
read_model_gis_gdb <- eventReactive(input$model_gis_gdb_load, {
  gdb.path <- input$model_gis_gdb_path
  gdb.name <- input$model_gis_gdb_name

  withProgress(message = "Loading GIS .gdb file", value = 0.3, {
    gis.file.gdb <- try(st_read(gdb.path, gdb.name, quiet = TRUE),
                        silent = TRUE)
    incProgress(0.4)

    gis.file.success <- isTruthy(gis.file.gdb)
    if (gis.file.success) {
      if (st_bbox(gis.file.gdb)[3] > 180) {
        incProgress(0.05, detail = "Polygon(s) span dateline; handling now")
        gis.file.gdb <- st_wrap_dateline(gis.file.gdb)
      }

      incProgress(detail = "Checking if model polygons are valid")
      if (!all(st_is_valid(gis.file.gdb))) {
        incProgress(0.1, detail = "Making model polygons valid")
        gis.file.gdb <- poly_valid_check(gis.file.gdb)
      }
      sf.list <- c(gis_model_check(gis.file.gdb), gdb.name)
      # gdb.name is included in list in case user types something else before
      #   actually importing the SDM into the app
    }
    incProgress(0.2, detail = "")
  })

  if (!gis.file.success) {
    NULL
  } else {
    sf.list
  }
})

output$read_model_gis_gdb_flag <- reactive({
  !is.null(read_model_gis_gdb())
})
outputOptions(output, "read_model_gis_gdb_flag", suspendWhenHidden = FALSE)

#######################################
### Process data and add it to list.all
create_sf_gis_gdb <- eventReactive(input$model_create_gis_gdb, {
  sf.list <- read_model_gis_gdb()

  pred.idx <- as.numeric(input$model_gis_gdb_names_pred)
  error.idx <- NA #as.numeric(input$model_gis_gdb_names_error)
  weight.idx <- as.numeric(input$model_gis_gdb_names_weight)

  model.name <- sf.list[[3]]
  pred.type <- input$model_gis_gdb_pred_type

  #### The code from this file is the same as in create_spdf_gis_shp() ####
  source(file.path("server_1_loadModels",
                   "server_1_loadModels_shpgdb_create_local.R"),
         local = TRUE, echo = FALSE, chdir = TRUE)

  "Model predictions loaded from GIS .gdb"
})
