### renderUI code for 'High Quality Maps' tab


###############################################################################
### Plot inputs

###########################################################
# XY range of plot

### Get extent of selected predictions
pretty_plot_range <- reactive({
  req(pretty_plot_xyz_count() == 1)
  model.preds <- pretty_plot_model_toplot()[[3]]
  round(extent(model.preds), 2)
})

### Render longitude, latitude min and max
output$pretty_plot_range_xmin_uiOut_num <- renderUI({
  val.default <- pretty_plot_range()@xmin
  numericInput("pretty_plot_range_xmin", h5("Longitude minimum"), 
               value = val.default, min = -180, max = 180)
})

output$pretty_plot_range_xmax_uiOut_num <- renderUI({
  val.default <- pretty_plot_range()@xmax
  numericInput("pretty_plot_range_xmax", h5("Longitude maximum"), 
               value = val.default, min = -180, max = 180)
})

output$pretty_plot_range_ymin_uiOut_num <- renderUI({
  val.default <- pretty_plot_range()@ymin
  numericInput("pretty_plot_range_ymin", h5("Latitude minimum"), 
               value = val.default, min = -90, max = 90)
})

output$pretty_plot_range_ymax_uiOut_num <- renderUI({
  val.default <- pretty_plot_range()@ymax
  numericInput("pretty_plot_range_ymax", h5("Latitude maximum"), 
               value = val.default, min = -90, max = 90)
})


###########################################################
### Title of plot
output$pretty_plot_title_uiOut_text <- renderUI({
  req(pretty_plot_xyz_count() == 1)
  list.selected <- pretty_plot_model_toplot()
  
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
  
  textInput("pretty_plot_title", h5("Plot title"), value = val.default)
})


###############################################################################
### Generate defualt filename for downloaded map
output$pretty_plot_download_name_uiOut_text <- renderUI({
  validate(need(pretty_plot_xyz_count() == 1, "Please select a model"))
  
  model.idx.null <- !pretty_plot_xyz_null()
  model.idx.list <- pretty_plot_xyz_list()
  
  ## Objects that are the same for multi- and single-map
  res.txt <- ifelse(input$pretty_plot_download_res == 1, "300ppi", "72ppi")
  file.ext <- switch(input$pretty_plot_download_format,
                     "1" = ".jpeg", "2" = ".pdf", "3" = ".png")
  
  ## Determine if map is a multi- or single-map
  if (length(unlist(model.idx.list)) > 1) {
    # Multi
    f.val <- paste0("Multi_", res.txt, file.ext)
  } else {
    # Single
    table.selected <- which(model.idx.null)
    idx.selected <- model.idx.list[[table.selected]]
    
    if (table.selected %in% c(1, 2)) {
      # Original or overlaid model predictions
      prefix <- ifelse(which(model.idx.null) == 1, "Original", "Overlaid")
      model.name <- vals$models.names[[idx.selected]]
      pred.name <- vals$models.data.names[[idx.selected]][1]
      
      f.val <- paste0(prefix, "_", model.name, "__", pred.name, "__", 
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
      ens.rescale.txt <- ifelse(grepl("Abund", ens.rescale.txt),
                                paste0("Abund",
                                       strsplit(ens.rescale.txt, ": ")[[1]][2],
                                       "_"),
                                switch(ens.rescale.txt,
                                       "None" = "None_",
                                       "Normalization" = "Norm_",
                                       "Standardization" = "Stand_",
                                       "Sum to 1" = "Sumto1_"))
      ens.idx.txt <- vals$ensemble.overlaid.idx[idx.selected]
      ens.idx.txt <- paste0(gsub(", ", "+", ens.idx.txt), "_")
      
      f.val <- paste0(ens.method.txt, ens.weights.txt, ens.rescale.txt,
                      ens.idx.txt, res.txt, file.ext)
    }
  }

  textInput("pretty_plot_download_name", h5("File name"), value = f.val)
})

###############################################################################
