### Code for reading in and processing model predictions from .shp or .gdb


###############################################################################
# gis.model.check() and gis.res.calc() are in ensLoadModels_funcs.R

###############################################################################
# Reactive functions for renderUIs

### Get names of data columns
shp_names_choice_input <- reactive({
  req(read_model_gis_shp())
  choice.input.names <- names(read_model_gis_shp()[[1]]@data)
  choice.input <- (1:length(choice.input.names))
  names(choice.input) <- choice.input.names
  
  choice.input
})

gdb_names_choice_input <- reactive({
  choice.input.names <- names(read_model_gis_gdb()[[1]]@data)
  choice.input <- (1:length(choice.input.names))
  names(choice.input) <- choice.input.names
  
  choice.input
})

### Identify NA predictions
model_gis_shp_NA_idx <- reactive({
  req(input$model_gis_shp_names_pred)
  data.shp <- read_model_gis_shp()[[1]]@data
  
  data.shp.pred <- data.shp[,as.numeric(input$model_gis_shp_names_pred)]
  
  na.which(data.shp.pred)
})

model_gis_gdb_NA_idx <- reactive({
  req(input$model_gis_gdb_names_pred)
  data.gdb <- read_model_gis_gdb()[[1]]@data
  
  data.gdb.pred <- data.gdb[,as.numeric(input$model_gis_gdb_names_pred)]
  
  na.which(data.gdb.pred)
})


###############################################################################
# Load and process data from shapefile

### Read in data and return SPolyDFs
read_model_gis_shp <- reactive({
  req(input$model_gis_shp_files)
  
  withProgress(message = "Loading GIS shapefile", value = 0.3, {
    gis.file.shp <- read.shp.in(input$model_gis_shp_files)
    incProgress(0.5)
    
    gis.file.success <- isTruthy(gis.file.shp)
    if(gis.file.success) gis.files.shp <- gis.model.check(gis.file.shp)
    incProgress(0.2)
  })
  
  if(!gis.file.success) {
    NULL
  } else {
    gis.files.shp    
  }
})

### Flag for if the shapefile was fully loaded and processed
output$read_model_gis_shp_flag <- reactive({
  !is.null(read_model_gis_shp())
})
outputOptions(output, "read_model_gis_shp_flag", suspendWhenHidden = FALSE)


#######################################
### Process data and add it to list.all
create_spdf_gis_shp <- eventReactive(input$model_create_gis_shp, {
  model.list <- read_model_gis_shp()
  
  pred.idx <- as.numeric(input$model_gis_shp_names_pred)
  error.idx <- NA #as.numeric(input$model_gis_shp_names_error)
  weight.idx <- as.numeric(input$model_gis_shp_names_weight)
  
  model.name <- strsplit(input$model_gis_shp_files$name[1], "\\.")[[1]][1]
  pred.type <- input$model_gis_shp_pred_type
  
  #### The code from this file is the same as in create_spdf_gis_gdb() ####
  source(file.path("server_1_loadModels", 
                   "server_1_loadModels_shpgdb_create_local.R"), 
         local = TRUE, echo = FALSE, chdir = TRUE)
  
  return("Model predictions loaded from GIS shapefile")
})


###############################################################################
# Load and process data from file geodatabase (.gdb)

### Read in data and return SPolyDFs
read_model_gis_gdb <- eventReactive(input$model_gis_gdb_load, {
  gdb.path <- input$model_gis_gdb_path
  gdb.name <- input$model_gis_gdb_name
  
  withProgress(message = "Loading GIS .gdb file", value = 0.3, {
    gis.file.gdb <- try(readOGR(gdb.path, gdb.name, verbose = FALSE), 
                        silent = TRUE)
    incProgress(0.5)
    
    gis.file.success <- isTruthy(gis.file.gdb)
    if(gis.file.success) gis.files.gdb <- gis.model.check(gis.file.gdb)
    incProgress(0.2)
  })
  
  if (!gis.file.success) {
    NULL
  } else {
    gis.files.gdb    
  }
})

output$read_model_gis_gdb_flag <- reactive({
  !is.null(read_model_gis_gdb())
})
outputOptions(output, "read_model_gis_gdb_flag", suspendWhenHidden = FALSE)

#######################################
### Process data and add it to list.all
create_spdf_gis_gdb <- eventReactive(input$model_create_gis_gdb, {
  model.list <- read_model_gis_gdb()
  
  pred.idx <- as.numeric(input$model_gis_gdb_names_pred)
  error.idx <- NA #as.numeric(input$model_gis_gdb_names_error)
  weight.idx <- as.numeric(input$model_gis_gdb_names_weight)
  
  model.name <- input$model_gis_gdb_name
  pred.type <- input$model_gis_gdb_pred_type
  
  #### The code from this file is the same as in create_spdf_gis_shp() ####
  source(file.path("server_1_loadModels", 
                   "server_1_loadModels_shpgdb_create_local.R"), 
         local = TRUE, echo = FALSE, chdir = TRUE)
  
  return("Model predictions loaded from GIS .gdb")
})
