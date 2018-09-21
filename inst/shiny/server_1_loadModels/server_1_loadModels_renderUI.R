### Items that are rendered for Load Predictions tab


###############################################################################
# Create input for user to select columns with data

###########################################################
# CSV
### Lon
output$model_csv_names_lon_uiOut_select <- renderUI({
  req(read_model_csv())
  choice.input <- csv_names_choice_input()
  selectInput("model_csv_names_lon", tags$h5("Column with longitude data"),
              choices = choice.input, selected = 1)
})

### Lat
output$model_csv_names_lat_uiOut_select <- renderUI({
  req(read_model_csv())
  choice.input <- csv_names_choice_input()
  selectInput("model_csv_names_lat", tags$h5("Column with latitude data"),
              choices = choice.input, selected = 2)
})

### Predictions
output$model_csv_names_pred_uiOut_select <- renderUI({
  req(read_model_csv())
  choice.input <- csv_names_choice_input()
  selectInput("model_csv_names_pred", tags$h5("Column with prediction data"),
              choices = choice.input, selected = 3)
})

### Prediction vaue type
output$model_csv_pred_type_uiOut_select <- renderUI({
  req(read_model_csv())
  selectInput("model_csv_pred_type", tags$h5("Prediction value type"),
              choices = list("Absolute density" = 1, "Relative density" = 2,
                             "Abundance" = 3),
              selected = 2)
})

### Weight
output$model_csv_names_weight_uiOut_select <- renderUI({
  req(read_model_csv())
  choice.input <- csv_names_choice_input()
  choice.input <- c("N/A - No pixel-level spatial weight data" = 1, choice.input + 1)
  selectInput("model_csv_names_weight",
              tags$h5("Column with weight data (optional)"),
              choices = choice.input, selected = NULL)
})

### Message with number of NA predictions for preds and weights
output$model_csv_NA_idx_uiOut_message <- renderUI({
  req(read_model_csv())

  model_csv_inf_func(
    req(input$model_csv_pred_type), model_csv_NA_idx_pred(),
    req(input$model_csv_names_weight), model_csv_NA_idx_weight()
  )
})


###########################################################
# GIS raster

### No pre/error/weight needed because raster only has predictions

### Prediction type value
output$model_gis_raster_pred_type_uiOut_select <- renderUI({
  req(read_model_gis_raster())
  selectInput("model_gis_raster_pred_type", tags$h5("Prediction value type"),
              choices = list("Absolute density" = 1, "Relative density" = 2,
                             "Abundance" = 3),
              selected = 2)
})

### Message with number of NA predictions
output$model_gis_raster_NA_idx_uiOut_message <- renderUI({
  req(read_model_gis_raster())

  temp <- ifelse(
    req(input$model_gis_raster_pred_type) != 3, NA,
    paste("Abundance value type: All prediction values will be divided",
          "by their prediction polygon area")
  )

  if (is.na(temp)) {
    na_pred_message(model_gis_raster_NA_idx_pred())
  } else {
    HTML(temp, "<br/>", "<br/>",
         na_pred_message(model_gis_raster_NA_idx_pred()))
  }
})


###########################################################
# GIS .shp

### Pred
output$model_gis_shp_names_pred_uiOut_select <- renderUI({
  req(read_model_gis_shp())
  choice.input <- shp_names_choice_input()
  selectInput("model_gis_shp_names_pred", tags$h5("Column with prediction data"),
              choices = choice.input, selected = 1)
})

### Prediction value type
output$model_gis_shp_pred_type_uiOut_select <- renderUI({
  req(read_model_gis_shp())
  selectInput("model_gis_shp_pred_type", tags$h5("Prediction value type"),
              choices = list("Absolute density" = 1, "Relative density" = 2,
                             "Abundance" = 3),
              selected = 2)
})

### Weight
output$model_gis_shp_names_weight_uiOut_select <- renderUI({
  req(read_model_gis_shp())
  choice.input <- shp_names_choice_input()
  choice.input <- c("N/A - No pixel-level spatial weight data" = 1, choice.input + 1)
  selectInput("model_gis_shp_names_weight",
              tags$h5("Column with weight data (optional)"),
              choices = choice.input, selected = 1)
})

### Message with number of NA predictions
output$model_gis_shp_NA_idx_uiOut_message <- renderUI({
  req(read_model_gis_shp())

  model_csv_inf_func(
    req(input$model_gis_shp_pred_type), model_gis_shp_NA_idx_pred(),
    req(input$model_gis_shp_names_weight), model_gis_shp_NA_idx_weight()
  )
})


###########################################################
# GIS .gdb

### Pred
output$model_gis_gdb_names_pred_uiOut_select <- renderUI({
  req(read_model_gis_gdb())
  choice.input <- gdb_names_choice_input()
  selectInput("model_gis_gdb_names_pred", tags$h5("Column with prediction data"),
              choices = choice.input, selected = 1)
})

### Prediction value type
output$model_gis_gdb_pred_type_uiOut_select <- renderUI({
  req(read_model_gis_gdb())
  selectInput("model_gis_gdb_pred_type", tags$h5("Prediction value type"),
              choices = list("Absolute density" = 1, "Relative density" = 2,
                             "Abundance" = 3),
              selected = 2)
})

### Weight
output$model_gis_gdb_names_weight_uiOut_select <- renderUI({
  req(read_model_gis_gdb())
  choice.input <- gdb_names_choice_input()
  choice.input <- c("N/A - No pixel-level spatial weight data" = 1, choice.input + 1)
  selectInput("model_gis_gdb_names_weight",
              tags$h5("Column with weight data (optional)"),
              choices = choice.input, selected = 1)
})

### Message with number of NA predictions
output$model_gis_gdb_NA_idx_uiOut_message <- renderUI({
  req(read_model_gis_gdb())

  model_csv_inf_func(
    req(input$model_gis_gdb_pred_type), model_gis_gdb_NA_idx_pred(),
    req(input$model_gis_gdb_names_weight), model_gis_gdb_NA_idx_weight()
  )
})


###############################################################################
# Create buttons for finishing loadinng models/adding models to app
#   These are here so that 'Model predictions loaded...' message resets
#   if a new file is loaded

### CSV
output$model_create_csv_uiOut_button <- renderUI ({
  req(read_model_csv())
  actionButton("model_create_csv", "Import original predictions")
})

### GIS raster
output$model_create_gis_raster_uiOut_button <- renderUI ({
  req(read_model_gis_raster())
  actionButton("model_create_gis_raster", "Import original predictions")
})

### GIS .shp
output$model_create_gis_shp_uiOut_button <- renderUI ({
  req(read_model_gis_shp()[[1]])
  actionButton("model_create_gis_shp", "Import original predictions")
})

### GIS .gdb
output$model_create_gis_gdb_uiOut_button <- renderUI ({
  req(read_model_gis_gdb()[[1]])
  actionButton("model_create_gis_gdb", "Import original predictions")
})


###############################################################################
# Loaded Model Predicitons section

### actionButton for interactive preview
output$model_preview_interactive_execute_uiOut_button <- renderUI({
  req(input$model_select_action == 1)

  validate(
    need(length(input$models_loaded_table_rows_selected) == 1,
         paste("You can only interactively preview one set of",
               "original predictions at a time")),
    errorClass = "validation2"
  )

  actionButton("model_preview_interactive_execute", "Plot interactive preview")
})

### Generate default filename for download of static preview
output$model_download_preview_name_uiOut_text <- renderUI({
  req(input$models_loaded_table_rows_selected)

  # Same for multi- and single- preview
  idx.selected <- as.numeric(input$models_loaded_table_rows_selected)
  perc.txt <- ifelse(input$model_download_preview_perc == 1,
                     "perc_", "values_")
  res.txt <- ifelse(input$model_download_preview_res == "1",
                    "300ppi", "72ppi")
  file.ext <- switch(input$model_download_preview_format,
                     "1" = ".jpeg", "2" = ".pdf", "3" = ".png")

  models.num <- length(input$models_loaded_table_rows_selected)
  if (models.num > 1) {
    # Multi
    f.val <- paste0(
      "eSDM_multi_", models.num, "_", perc.txt, res.txt, file.ext
    )

  } else {
    # Single
    model.name <- vals$models.names[[idx.selected]]
    pred.name <- vals$models.data.names[[idx.selected]][1]

    f.val <- paste0(
      "eSDM_", model.name, "__", pred.name, "__", perc.txt, res.txt, file.ext
    )
  }

  textInput("model_download_preview_name", tags$h5("Filename"), value = f.val)
})

output$model_download_preview_execute_uiOut_download <- renderUI({
  if (input$model_download_preview_dim == 2) {
    validate(
      need(isTruthy(session$clientData$output_model_preview_plot_width) &&
             isTruthy(session$clientData$output_model_preview_plot_height),
           paste("You must plot a static preview before downloading a file",
                 "with these dimensions")),
      errorClass = "validation2"
    )
  }

  downloadButton("model_download_preview_execute", "Download")
})


###############################################################################
