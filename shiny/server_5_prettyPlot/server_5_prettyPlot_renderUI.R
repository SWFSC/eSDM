### renderUI code for 'High Quality Maps' tab


###############################################################################
### Pretty plot inputs

###########################################################
# Range and projection of map

### Select object with desired projection for map
output$pretty_plot_proj_idx_uiOut_select <- renderUI({
  req(vals$models.names)
  
  choices.list.names <- vals$models.names
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names
  
  selectInput("pretty_plot_proj_idx", 
              tags$h5("Filename of original model predictions", 
                      "with desired projection for map"), 
              choices = choices.list, selected = 1)
})

### Get extent of selected predictions
pretty_plot_range <- reactive({
  req(pretty_plot_models_idx_count() == 1)
  model.preds <- pretty_plot_models_toplot()[[3]]
  round(extent(model.preds), 2)
})

### Render longitude, latitude min and max
output$pretty_plot_range_xmin_uiOut_num <- renderUI({
  val.default <- pretty_plot_range()@xmin
  numericInput("pretty_plot_range_xmin", tags$h5("Longitude minimum"), 
               value = val.default, min = -180, max = 180)
})

output$pretty_plot_range_xmax_uiOut_num <- renderUI({
  val.default <- pretty_plot_range()@xmax
  numericInput("pretty_plot_range_xmax", tags$h5("Longitude maximum"), 
               value = val.default, min = -180, max = 180)
})

output$pretty_plot_range_ymin_uiOut_num <- renderUI({
  val.default <- pretty_plot_range()@ymin
  numericInput("pretty_plot_range_ymin", tags$h5("Latitude minimum"), 
               value = val.default, min = -90, max = 90)
})

output$pretty_plot_range_ymax_uiOut_num <- renderUI({
  val.default <- pretty_plot_range()@ymax
  numericInput("pretty_plot_range_ymax", tags$h5("Latitude maximum"), 
               value = val.default, min = -90, max = 90)
})


###########################################################
### Title of plot
output$pretty_plot_title_uiOut_text <- renderUI({
  req(pretty_plot_models_idx_count() == 1)
  list.selected <- pretty_plot_models_toplot()
  
  table.idx <- list.selected[[1]]
  model.idx <- list.selected[[2]]
  
  val.default <- ifelse(table.idx == 3, 
                        paste("Ensembling method:", 
                              vals$ensemble.method[model.idx], "||", 
                              "Rescaling method:", 
                              vals$ensemble.rescaling[model.idx]), 
                        paste("Model file:", 
                              vals$models.names[model.idx], "||", 
                              "Data header:", 
                              vals$models.data.names[[model.idx]][1]))
  
  textInput("pretty_plot_title", tags$h5("Plot title"), value = val.default)
})


###########################################################
# Color scheme inputs

### Color palette
# If 'plot predictions as percentages' is selected, then remove some options
output$pretty_plot_color_palette_uiOut_select <- renderUI({
  choices.list <- list("Default: blue to white to red" = 1, 
                       "RColorBrewer: Spectral (rainbow)" = 2, 
                       "RColorBrewer: YlGnBu" = 3, 
                       "viridis: viridis" = 4, 
                       "viridis: inferno" = 5, 
                       "dichromat: DarkRedtoBlue" = 6)
  
  if (input$pretty_plot_color_perc == 1) choices.list <- choices.list[-c(3, 6)]
  
  selectInput("pretty_plot_color_palette", tags$h5("Color palette"), 
              choices = choices.list, selected = NULL)
})

### Number of colors
# Selectively give user input control, depending on perc/palette
# input$pretty_plot_color_palette doesn't change if helpText() is outputted, 
# but colorschem.num value is hardcoded for those situations in 
# pretty_plot_colorscheme_list()
output$pretty_plot_color_num_uiOut_num <- renderUI({
  if (input$pretty_plot_color_perc == 1) {
    helpText("The number of colors must be ten when using", 
             "relative percentages of predictions")
    
  } else if (input$pretty_plot_color_palette == 1) {
    helpText("The number of colors must be 10 when", 
             "using this color palette")
    
  } else if (input$pretty_plot_color_palette == 2) {
    numericInput("pretty_plot_color_num", 
                 tags$h5("Number of colors (Min: 3; Max: 11)"), 
                 value = 11, step = 1, min = 3, max = 11)
    
  } else if (input$pretty_plot_color_palette == 3) {
    numericInput("pretty_plot_color_num", 
                 tags$h5("Number of colors (Min: 3; Max: 9)"), 
                 value = 9, step = 1, min = 3, max = 9)
    
  } else if (input$pretty_plot_color_palette == 6) {
    helpText("The number of colors must be 12 when", 
             "using this color palette")
    
  } else {
    numericInput("pretty_plot_color_num", tags$h5("Number of colors"), 
                 value = 10, step = 1, min = 1)
  }
})


###########################################################
### Checkbox for including other polygons
# For this beta version: hardcoded for only study area and land polys
output$pretty_plot_other_obj_which_uiOut_selectize <- renderUI({
  bound.poly <- vals$overlay.bound
  land.poly <- vals$overlay.land
  
  validate(
    need((!is.null(bound.poly)) | (!is.null(land.poly)), 
         paste("Error: Neither a study area polygon nor a", 
               "land area polygon is loaded"))
  )
  
  choices.list <- list()
  if (!is.null(bound.poly)) {
    choices.list <- c(choices.list, "Study area polygon" = 1)
  }
  if (!is.null(land.poly)) {
    choices.list <- c(choices.list, "Land polygon" = 2)
  }
  
  selectizeInput("pretty_plot_other_obj_which", 
                 tags$h5("Include selected polygons in map"), 
                 choices = choices.list, selected = NULL, multiple = TRUE)
})


###############################################################################
### Generate defualt filename for downloaded map
output$pretty_plot_download_name_uiOut_text <- renderUI({
  validate(
    need(pretty_plot_models_idx_count() == 1, 
         "Error: Please select exactly one set of model predictions")
  )
  
  model.idx.null <- !pretty_plot_tables_null()
  model.idx.list <- pretty_plot_models_idx_list()
  
  ## Objects that are the same for multi- and single-map
  res.txt <- ifelse(input$pretty_plot_download_res == 1, "300ppi", "72ppi")
  file.ext <- switch(input$pretty_plot_download_format, 
                     "1" = ".jpeg", "2" = ".pdf", "3" = ".png")
  
  ## Determine if map is a multi- or single-map and name as appropriate
  if (length(unlist(model.idx.list)) > 1) {
    # Multi
    f.val <- paste0("eSDM_multi_", res.txt, file.ext)
  } else {
    # Single
    table.selected <- which(model.idx.null)
    idx.selected <- model.idx.list[[table.selected]]
    
    if (table.selected %in% c(1, 2)) {
      # Original or overlaid model predictions
      prefix <- ifelse(which(model.idx.null) == 1, "Original", "Overlaid")
      model.name <- vals$models.names[[idx.selected]]
      pred.name <- vals$models.data.names[[idx.selected]][1]
      
      f.val <- paste0("eSDM_", prefix, "_", model.name, "__", pred.name, "__", 
                      res.txt, file.ext)
    } else {
      # Ensemble model predictions
      ens.method.txt <- switch(vals$ensemble.method[idx.selected], 
                               "Unweighted" = "UnW_", "Weighted" = "W_")
      ens.weights.txt <- vals$ensemble.weights[idx.selected]
      ens.weights.txt <- ifelse(is.na(ens.weights.txt), 
                                "", 
                                paste0(gsub(", ", "+", ens.weights.txt), "_"))
      ens.rescale.txt <- vals$ensemble.rescaling[idx.selected]
      ens.rescale.txt <- ifelse(
        grepl("Abund", ens.rescale.txt), 
        paste0("Abund", strsplit(ens.rescale.txt, ": ")[[1]][2], "_"), 
        switch(ens.rescale.txt, "None" = "None_", "Normalization" = "Norm_", 
               "Standardization" = "Stand_", "Sum to 1" = "Sumto1_"))
      ens.idx.txt <- vals$ensemble.overlaid.idx[idx.selected]
      ens.idx.txt <- paste0(gsub(", ", "+", ens.idx.txt), "_")
      
      f.val <- paste0("eSDM_", ens.method.txt, ens.weights.txt, ens.rescale.txt, 
                      ens.idx.txt, res.txt, file.ext)
    }
  }
  
  textInput("pretty_plot_download_name", tags$h5("File name"), value = f.val)
})

###############################################################################
