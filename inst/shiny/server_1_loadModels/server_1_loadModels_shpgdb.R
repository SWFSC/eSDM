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

  withProgress(message = "Loading GIS shapefile", value = 0.4, {
    gis.file.shp <- read.shp.shiny(input$model_gis_shp_files)
    incProgress(0.6)
  })

  if(isTruthy(gis.file.shp)) {
    list(
      gis.file.shp, strsplit(input$model_gis_shp_files$name[1], "\\.")[[1]][1]
    )
  } else {
    NULL
  }
})

### Flag for if the shapefile was fully loaded and processed
output$read_model_gis_shp_flag <- reactive({
  isTruthy(read_model_gis_shp())
})
outputOptions(output, "read_model_gis_shp_flag", suspendWhenHidden = FALSE)


#######################################
### Process data and add it to list.all
create_sf_gis_shp <- eventReactive(input$model_create_gis_shp, {
  # Prep for create_local code
  gis.file <- read_model_gis_shp()[[1]]

  pred.idx <- as.numeric(input$model_gis_shp_names_pred)
  error.idx <- NA #as.numeric(input$model_gis_shp_names_error)
  weight.idx <- as.numeric(input$model_gis_shp_names_weight)

  # Ensure that weight values are between 0 and 1
  if (weight.idx != 1) {
    data.all <- st_set_geometry(read_model_gis_shp()[[1]], NULL)
    data.weight <- data.all[, (weight.idx - 1)]
    validate(
      need((max(data.weight, na.rm = TRUE) <= 1) &
             (min(data.weight, na.rm = TRUE) >= 0),
           "Error: Values in 'Weight' column must be between 0 and 1")
    )
  }

  # Continue create_local code prep
  model.name <-read_model_gis_shp()[[2]]
  pred.type <- input$model_gis_shp_pred_type

  #### The code from this file is the same as in create_spdf_gis_gdb() ####
  source(file.path(
    "server_1_loadModels", "server_1_loadModels_shpgdb_create_local.R"
  ), local = TRUE, echo = FALSE, chdir = TRUE)

  "Model predictions loaded from GIS shapefile"
})


###############################################################################
# Load and process data from file geodatabase (.gdb)

### Read in data and return sf object
read_model_gis_gdb <- eventReactive(input$model_gis_gdb_load, {
  gdb.path <- input$model_gis_gdb_path
  gdb.name <- input$model_gis_gdb_name

  withProgress(message = "Loading GIS .gdb file", value = 0.4, {
    gis.file.gdb <- try(st_read(gdb.path, gdb.name, quiet = TRUE),
                        silent = TRUE)
    incProgress(0.6)
  })

  if (isTruthy(gis.file.gdb)) {
    list(gis.file.gdb, gdb.name)
  } else {
    NULL
  }
})

output$read_model_gis_gdb_flag <- reactive({
  isTruthy(read_model_gis_gdb())
})
outputOptions(output, "read_model_gis_gdb_flag", suspendWhenHidden = FALSE)

#######################################
### Process data and add it to list.all
create_sf_gis_gdb <- eventReactive(input$model_create_gis_gdb, {
  # Prep for create_local code
  gis.file <- read_model_gis_gdb()[[1]]

  pred.idx <- as.numeric(input$model_gis_gdb_names_pred)
  error.idx <- NA #as.numeric(input$model_gis_gdb_names_error)
  weight.idx <- as.numeric(input$model_gis_gdb_names_weight)

  # Ensure that weight values are between 0 and 1
  if (weight.idx != 1) {
    data.all <- st_set_geometry(read_model_gis_gdb()[[1]], NULL)
    data.weight <- data.all[, (weight.idx - 1)]
    validate(
      need((max(data.weight, na.rm = TRUE) <= 1) &
             (min(data.weight, na.rm = TRUE) >= 0),
           "Error: Values in 'Weight' column must be between 0 and 1")
    )
  }

  # Continue create_local code prep
  model.name <- read_model_gis_gdb()[[2]]
  pred.type <- input$model_gis_gdb_pred_type

  #### The code from this file is the same as in create_spdf_gis_shp() ####
  source(file.path(
    "server_1_loadModels", "server_1_loadModels_shpgdb_create_local.R"
  ), local = TRUE, echo = FALSE, chdir = TRUE)

  "Model predictions loaded from GIS .gdb"
})
