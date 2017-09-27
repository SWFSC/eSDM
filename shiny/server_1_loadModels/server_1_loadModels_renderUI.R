### Items that are rendered for Load Model Predictions tab


###############################################################################
# Create input for user to select columns with data

###########################################################
# CSV
### Lon
output$model_csv_names_lon_uiOut_select <- renderUI({
  req(read_model_csv())
  
  choice.input <- csv_names_choice_input()
  
  selectInput("model_csv_names_lon", h5("Column with longitude data"),
              choices = choice.input, selected = 1)
})

### Lat
output$model_csv_names_lat_uiOut_select <- renderUI({
  req(read_model_csv())
  
  choice.input <- csv_names_choice_input()
  
  selectInput("model_csv_names_lat", h5("Column with latitude data"),
              choices = choice.input, selected = 2)
})

### Predictions
output$model_csv_names_pred_uiOut_select <- renderUI({
  req(read_model_csv())
  
  choice.input <- csv_names_choice_input()
  
  selectInput("model_csv_names_pred", h5("Column with prediction data"),
              choices = choice.input, selected = 3)
})

### Prediction vaue type
output$model_csv_pred_type_uiOut_select <- renderUI({
  req(read_model_csv())
  
  selectInput("model_csv_pred_type", h5("Prediction value type"), 
              choices = list("Absolute density" = 1, "Relative density" = 2), 
              selected = 2)
})

# ### Error
# output$model_csv_names_error_uiOut_select <- renderUI({
#   req(read_model_csv())
#   
#   choice.input <- csv_names_choice_input()
#   choice.input <- c("N/A" = 1, choice.input + 1)
#   
#   selectInput("model_csv_names_error", 
#               h5("Column with error data (optional)"), 
#               choices = choice.input, selected = NULL)
# })

### Weight
output$model_csv_names_weight_uiOut_select <- renderUI({
  req(read_model_csv())
  
  choice.input <- csv_names_choice_input()
  choice.input <- c("N/A" = 1, choice.input + 1)
  
  selectInput("model_csv_names_weight", 
              h5("Column with weight data (optional)"),
              choices = choice.input, selected = NULL)
})

### Message with number of NA predictions
output$model_csv_NA_idx_uiOut_message <- renderUI({
  req(read_model_csv())
  
  na.idx <- model_csv_NA_idx()
  
  na.which.message(na.idx)
})


###########################################################
# GIS raster

### No pre/error/weight needed because raster only has predictions

### Prediction type value
output$model_gis_raster_pred_type_uiOut_select <- renderUI({
  req(read_model_gis_raster())
  
  selectInput("model_gis_raster_pred_type", h5("Prediction value type"), 
              choices = list("Absolute density" = 1, "Relative density" = 2), 
              selected = 2)
})

### Message with number of NA predictions
output$model_gis_raster_NA_idx_uiOut_message <- renderUI({
  req(read_model_gis_raster())
  
  na.idx <- model_gis_raster_NA_idx()
  
  na.which.message(na.idx)
})


###########################################################
# GIS .shp

### Pred
output$model_gis_shp_names_pred_uiOut_select <- renderUI({
  req(read_model_gis_shp())
  
  choice.input <- shp_names_choice_input()
  
  selectInput("model_gis_shp_names_pred", h5("Column with prediction data"),
              choices = choice.input, selected = 1)
})

### Prediction value type
output$model_gis_shp_pred_type_uiOut_select <- renderUI({
  req(read_model_gis_shp())
  
  selectInput("model_gis_shp_pred_type", h5("Prediction value type"), 
              choices = list("Absolute density" = 1, "Relative density" = 2),
              selected = 2)
})

# ### Error
# output$model_gis_shp_names_error_uiOut_select <- renderUI({
#   req(read_model_gis_shp())
#   
#   choice.input <- shp_names_choice_input()
#   choice.input <- c("N/A" = 1, choice.input + 1)
#   
#   selectInput("model_gis_shp_names_error", 
#               h5("Column with error data (optional)"),
#               choices = choice.input, selected = 1)
# })

### Weight
output$model_gis_shp_names_weight_uiOut_select <- renderUI({
  req(read_model_gis_shp())
  
  choice.input <- shp_names_choice_input()
  choice.input <- c("N/A" = 1, choice.input + 1)
  
  selectInput("model_gis_shp_names_weight", 
              h5("Column with weight data (optional)"),
              choices = choice.input, selected = 1)
})

### Message with number of NA predictions
output$model_gis_shp_NA_idx_uiOut_message <- renderUI({
  req(read_model_gis_shp())
  
  na.idx <- model_gis_shp_NA_idx()
  
  na.which.message(na.idx)
})


###########################################################
# GIS .gdb

### Pred
output$model_gis_gdb_names_pred_uiOut_select <- renderUI({
  req(read_model_gis_gdb())
  
  choice.input <- gdb_names_choice_input()
  
  selectInput("model_gis_gdb_names_pred", h5("Column with prediction data"),
              choices = choice.input, selected = 1)
})

### Prediction value type
output$model_gis_gdb_pred_type_uiOut_select <- renderUI({
  req(read_model_gis_gdb())
  
  selectInput("model_gis_gdb_pred_type", h5("Prediction value type"), 
              choices = list("Absolute density" = 1, "Relative density" = 2), 
              selected = 2)
})

# ### Error
# output$model_gis_gdb_names_error_uiOut_select <- renderUI({
#   req(read_model_gis_gdb())
#   
#   choice.input <- gdb_names_choice_input()
#   choice.input <- c("N/A" = 1, choice.input + 1)
#   
#   selectInput("model_gis_gdb_names_error", 
#               h5("Column with error data (optional)"),
#               choices = choice.input, selected = 1)
# })

### Weight
output$model_gis_gdb_names_weight_uiOut_select <- renderUI({
  req(read_model_gis_gdb())
  
  choice.input <- gdb_names_choice_input()
  choice.input <- c("N/A" = 1, choice.input + 1)
  
  selectInput("model_gis_gdb_names_weight", 
              h5("Column with weight data (optional)"),
              choices = choice.input, selected = 1)
})

### Message with number of NA predictions
output$model_gis_gdb_NA_idx_uiOut_message <- renderUI({
  req(read_model_gis_gdb())
  
  na.idx <- model_gis_gdb_NA_idx()
  
  na.which.message(na.idx)
})


###############################################################################
# Create buttons for finishing loadinng models/adding models to app

### csv
output$model_create_csv_uiOut_button <- renderUI ({
  req(read_model_csv(), input$model_csv_names_weight)
  
  # Ensure that weight values are between 0 and 1
  weight.idx <- as.numeric(input$model_csv_names_weight) - 1
  if (weight.idx != 0) {
    data.all <- read_model_csv()[[2]]
    data.weight <- data.all[,weight.idx]
    validate(
      need((max(data.weight, na.rm = TRUE) <= 1) & 
             (min(data.weight, na.rm = TRUE) >= 0), 
           "Values in 'Weight' column must be between 0 and 1")
    )
  }
  
  actionButton("model_create_csv", "Load model predictions")
})

### GIS raster
output$model_create_gis_raster_uiOut_button <- renderUI ({
  req(read_model_gis_raster())
  
  actionButton("model_create_gis_raster", "Load model predictions")
})

### GIS .shp
output$model_create_gis_shp_uiOut_button <- renderUI ({
  req(read_model_gis_shp()[[1]], input$model_gis_shp_names_weight)
  
  # Ensure that weight values are between 0 and 1
  weight.idx <- as.numeric(input$model_gis_shp_names_weight) - 1
  if (weight.idx != 0) {
    data.all <- read_model_gis_shp()[[1]]@data
    data.weight <- data.all[,weight.idx]
    validate(
      need((max(data.weight, na.rm = TRUE) <= 1) & 
             (min(data.weight, na.rm = TRUE) >= 0),
           "Values in 'Weight' column must be between 0 and 1")
    )
  }
  
  actionButton("model_create_gis_shp", "Load model predictions")
})

### GIS .gdb
output$model_create_gis_gdb_uiOut_button <- renderUI ({
  req(read_model_gis_gdb()[[1]], input$model_gis_gdb_names_weight)
  
  # Ensure that weight values are between 0 and 1
  weight.idx <- as.numeric(input$model_gis_gdb_names_weight) - 1
  if (weight.idx != 0) {
    data.all <- read_model_gis_gdb()[[1]]@data
    data.weight <- data.all[,weight.idx]
    validate(
      need((max(data.weight, na.rm = TRUE) <= 1) & 
             (min(data.weight, na.rm = TRUE) >= 0),
           "Values in 'Weight' column must be between 0 and 1")
    )
  }
  
  actionButton("model_create_gis_gdb", "Load model predictions")
})


###############################################################################
### Generate default filename for download of model preditcions preview
output$model_download_preview_name_uiOut_text <- renderUI({
  req(input$models_loaded_table_rows_selected)
  
  # Same for multi- and single- preview
  idx.selected <- as.numeric(input$models_loaded_table_rows_selected)
  
  perc.txt <- ifelse(input$model_download_preview_perc == 1, 
                     "perc_", "values_")
  res.txt <- ifelse(input$model_download_preview_res == "1", 
                    "300ppi", "72ppi")
  # res.txt <- ifelse(input$download_plot_format == 2, "", res.txt)
  file.ext <- switch(input$model_download_preview_format, 
                     "1" = ".jpeg", "2" = ".pdf", "3" = ".png")
  
  models.num <- length(input$models_loaded_table_rows_selected)
  if(models.num > 1) {
    # Multi
    f.val <- paste0("Multi_", models.num, "_", perc.txt, res.txt, file.ext)
  } else {
    # Single
    model.name <- vals$models.names[[idx.selected]]
    pred.name <- vals$models.data.names[[idx.selected]][1]
    
    f.val <- paste0(model.name, "__", pred.name, "__", 
                    perc.txt, res.txt, file.ext)
  }
  
  textInput("model_download_preview_name", h5("Filename"), value = f.val)
})


###############################################################################
