### Code for importing predictions from .shp or .gdb file


###############################################################################
# gis.model.check() and gis.res.calc() are in '..._loadModels_func.R'

###############################################################################
# Reactive functions for renderUIs

#----------------------------------------------------------
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


#----------------------------------------------------------
### Identify NA prediction values
model_gis_shp_NA_idx_pred <- reactive({
  data.shp <- st_set_geometry(req(read_model_gis_shp())[[1]], NULL)

  data.col <- as.numeric(req(input$model_gis_shp_names_pred))
  req(data.col <= ncol(data.shp))

  na_which(data.shp[, data.col])
})

model_gis_gdb_NA_idx_pred <- reactive({
  data.gdb <- st_set_geometry(req(read_model_gis_gdb())[[1]], NULL)

  data.col <- as.numeric(req(input$model_gis_gdb_names_pred))
  req(data.col <= ncol(data.gdb))

  na_which(data.gdb[, data.col])
})


#----------------------------------------------------------
### Identify NA uncertainty values
model_gis_shp_NA_idx_var <- reactive({
  data.shp <- st_set_geometry(req(read_model_gis_shp())[[1]], NULL)
  var.col <- as.numeric(req(input$model_gis_shp_names_var))
  req((var.col - 1) <= ncol(data.shp))

  if (var.col > 1) {
    na_which(data.shp[, var.col - 1])
  } else {
    NA
  }
})

model_gis_gdb_NA_idx_var <- reactive({
  data.gdb <- st_set_geometry(req(read_model_gis_gdb())[[1]], NULL)
  var.col <- as.numeric(req(input$model_gis_gdb_names_var))
  req((var.col - 1) <= ncol(data.gdb))

  if (var.col > 1) {
    na_which(data.gdb[, var.col - 1])
  } else {
    NA
  }
})


#----------------------------------------------------------
### Identify NA weight values
model_gis_shp_NA_idx_weight <- reactive({
  data.shp <- st_set_geometry(req(read_model_gis_shp())[[1]], NULL)
  weight.col <- as.numeric(req(input$model_gis_shp_names_weight))
  req((weight.col - 1) <= ncol(data.shp))

  if (weight.col > 1) {
    na_which(data.shp[, weight.col - 1])
  } else {
    NA
  }
})

model_gis_gdb_NA_idx_weight <- reactive({
  data.gdb <- st_set_geometry(req(read_model_gis_gdb())[[1]], NULL)
  weight.col <- as.numeric(req(input$model_gis_gdb_names_weight))
  req((weight.col - 1) <= ncol(data.gdb))

  if (weight.col > 1) {
    na_which(data.gdb[, weight.col - 1])
  } else {
    NA
  }
})


###############################################################################
# Upload and process data from shapefile

#------------------------------------------------------------------------------
### Read in data and return sf object
read_model_gis_shp <- reactive({
  req(input$model_gis_shp_files)

  withProgress(message = "Uploading shapefile", value = 0.4, {
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

### Flag for if the shapefile was properly uploaded
output$read_model_gis_shp_flag <- reactive({
  isTruthy(read_model_gis_shp())
})
outputOptions(output, "read_model_gis_shp_flag", suspendWhenHidden = FALSE)


#------------------------------------------------------------------------------
### Process shapefile data
create_sf_gis_shp <- eventReactive(input$model_create_gis_shp, {
  # Prep for create_local code
  gis.file <- read_model_gis_shp()[[1]]

  pred.idx   <- as.numeric(input$model_gis_shp_names_pred)
  var.idx    <- as.numeric(input$model_gis_shp_names_var)
  weight.idx <- as.numeric(input$model_gis_shp_names_weight)

  # Check that pred and weight data are valid
  gis.file <- check_pred_var_weight(
    gis.file, pred.idx, ifelse(var.idx == 1, NA, var.idx - 1),
    ifelse(weight.idx == 1, NA, weight.idx - 1),
    model_gis_shp_NA_idx_pred(), model_gis_shp_NA_idx_var(),
    model_gis_shp_NA_idx_weight()
  )

  # Continue create_local code prep
  model.name <-read_model_gis_shp()[[2]]
  pred.type <- input$model_gis_shp_pred_type
  var.type <- input$model_gis_shp_var_type
  prog.message <- "Importing predictions from shapefile"

  #### The code from this file is the same as in create_sf_gis_gdb() ####
  source(file.path(
    "server_1_loadModels", "server_1_loadModels_shpgdb_create_local.R"
  ), local = TRUE, echo = FALSE, chdir = TRUE)

  "Predictions imported from shapefile"
})


###############################################################################
# Upload and process data from file geodatabase (.gdb) feature class

### Read in data and return sf object
read_model_gis_gdb <- eventReactive(input$model_gis_gdb_load, {
  gdb.path <- input$model_gis_gdb_path
  gdb.name <- input$model_gis_gdb_name

  withProgress(message = "Uploading feature class", value = 0.4, {
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


#------------------------------------------------------------------------------
### Process feature class data
create_sf_gis_gdb <- eventReactive(input$model_create_gis_gdb, {
  # Prep for create_local code
  gis.file <- read_model_gis_gdb()[[1]]

  pred.idx   <- as.numeric(input$model_gis_gdb_names_pred)
  var.idx    <- as.numeric(input$model_gis_gdb_names_var)
  weight.idx <- as.numeric(input$model_gis_gdb_names_weight)

  # Check that pred and weight data are valid
  gis.file <- check_pred_var_weight(
    gis.file, pred.idx, ifelse(var.idx == 1, NA, var.idx - 1),
    ifelse(weight.idx == 1, NA, weight.idx - 1),
    model_gis_gdb_NA_idx_pred(), model_gis_gdb_NA_idx_var(),
    model_gis_gdb_NA_idx_weight()
  )

  # Continue create_local code prep
  model.name <- read_model_gis_gdb()[[2]]
  pred.type <- input$model_gis_gdb_pred_type
  var.type <- input$model_gis_gdb_var_type
  prog.message <- "Importing predictions from feature class"

  #### The code from this file is the same as in create_sf_gis_shp() ####
  source(file.path(
    "server_1_loadModels", "server_1_loadModels_shpgdb_create_local.R"
  ), local = TRUE, echo = FALSE, chdir = TRUE)

  "Predictions imported from feature class"
})
