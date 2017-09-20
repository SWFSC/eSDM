### renderUI() functions for Create (simple) Ensemble section


###############################################################################
# 'Create Ensemble Predictions' box, weighted ensembling widgets

###########################################################
# Method 1

### Weights for a manually weighted ensemble
output$create_ens_weight_manual_uiOut_text <- renderUI ({
  models.num <- length(vals$overlaid.models)
  
  if (input$create_ens_table_subset) {
    models.num <- length(input$create_ens_datatable_rows_selected)
  }
  
  text.val <- paste(rep("1.0", models.num), collapse = ", ")
  textInput("create_ens_weight_manual", h5("Ensemble weights"), 
            value = text.val, width = "40%")
})


###########################################################
# Method 2

### Checkboxes with metrics that have been calculated and thus can be weights
output$create_ens_weights_metric_uiOut_radio <- renderUI({
  choice.input <- vals$eval.metrics.names
  
  radioButtons("create_ens_weights_metric", 
               h5("Metric to use for weights"),
               choices = choice.input, selected = NULL)
})


###########################################################
# Method 3
### None


###########################################################
# Method 4: Load polys with weights and apply them to overlaid predictions

#######################################
### Widget to select overlaid predictions to which to apply loaded poly weights
output$create_ens_weights_poly_model_uiOut_selectize <- renderUI({
  models.which <- seq_along(vals$overlaid.models)
  
  if (input$create_ens_table_subset) {
    ens.selected <- input$create_ens_datatable_rows_selected
    models.which <- models.which[models.which %in% ens.selected]
  }

  input.val <- as.list(paste("Overlaid", models.which))
  selectizeInput("create_ens_weights_poly_model", 
                 h5("Overlaid predictions to which to apply polygon weights"),
                 choices = input.val, selected = NULL, multiple = TRUE)
})


#######################################
### Widget to select overlaid predictions for which to plot preds and wpolys
output$create_ens_weights_poly_preview_model_uiOut_select <- renderUI({
  models.which <- seq_along(vals$overlaid.models)
  
  if (input$create_ens_table_subset) {
    ens.selected <- input$create_ens_datatable_rows_selected
    models.which <- models.which[models.which %in% ens.selected]
  }
  
  input.val <- as.list(paste("Overlaid", models.which))
  choices.list <- as.list(models.which)
  names(choices.list) <- input.val
  
  selectInput("create_ens_weights_poly_preview_model", 
              h5("Overlaid predictions to preview"),
              choices = choices.list, selected = NULL)
})


#######################################
# Select name of column that has weight data

### shp
output$create_ens_weights_poly_shp_field_uiOut_select <- renderUI({
  selectInput("create_ens_weights_poly_shp_field", 
              h5("Name of column with weight(s)"), 
              choices = names(create_ens_weights_poly_shp_read()[[1]]), 
              selected = NULL)
})

### gdb
output$create_ens_weights_poly_gdb_field_uiOut_select <- renderUI({
  selectInput("create_ens_weights_poly_gdb_field", 
              h5("Name of column with weight(s)"), 
              choices = names(create_ens_weights_poly_gdb_read()[[1]]), 
              selected = NULL)
})


#######################################
### Select loaded polygons to delete
output$create_ens_weights_poly_remove_choices_uiOut_select <- renderUI({
  req(!all(sapply(vals$ens.over.wpoly.filename, is.null)))
  
  poly.table <- create_ens_weights_poly_table()
  
  choices.list.names <- apply(poly.table, 1, function(i) {
    x <- unlist(strsplit(i[2], ", "))
    y <- unlist(strsplit(i[3], ", "))
    z <- unlist(strsplit(i[4], ", "))
    
    if (length(x) != 0) {
      paste(i[1], x, y, z, sep = " || ")
    } else {
      NULL
    }
  })
  
  model.which <- which(!sapply(choices.list.names, is.null))
  poly.num <- lapply(choices.list.names[model.which], seq_along)

  choices.list <- unlist(mapply(function(i, j) {
    paste(i, j, sep = ", ")
  }, 
  model.which, poly.num))
  names(choices.list) <- unlist(choices.list.names)
  
  selectizeInput("create_ens_weights_poly_remove_choices", 
                 h5("Select loaded weight polygon(s) to remove"), 
                 choices = choices.list, selected = NULL, multiple = TRUE)
})


###############################################################################
# 'Create Ensemble Predictions' box, other widgets

### Widget with options for rescaling
output$create_ens_rescale_type_uiOut_radio <- renderUI({
  pred.type <- vals$models.pred.type
  
  choices.list <- list("None" = 1, "Abundance" = 2, "Normalization" = 3,
                       "Standardization" = 4, "Sum to 1" = 5)
  if (!all(pred.type == "1")) choices.list <- choices.list[2:5]
  
  radioButtons("create_ens_rescale_type", "Rescaling method", 
               choices = choices.list, selected = choices.list[[1]])
})


###############################################################################
# 'Created Ensemble Predictions' box

### actionButton to calculate abundances
output$ens_calc_abund_execute_uiOut_button <- renderUI({
  ens.rows <- input$ens_datatable_ensembles_rows_selected
  req(ens.rows)

  ens.rescalings <- vals$ensemble.rescaling[ens.rows]
  rescaling.abund.bad <- c("Normalization", "Standardization", "Sum to 1")
  
  validate(
    need(all(!(ens.rescalings %in% rescaling.abund.bad)), 
         paste("Abundance cannot be reasonably calculated for ensembles", 
               "made with predictions rescaled using the 'Normalization',", 
               "'Standardization', or 'Sum to 1' methods"))
  )
  
  actionButton("ens_calc_abund_execute", 
               "Calculate abundance for selected ensembles")
})

### textInput with default filename for download of ensemble preview
output$ens_download_preview_name_uiOut_text <- renderUI({
  req(input$ens_datatable_ensembles_rows_selected)
  
  # Same for multi- and single- preview
  perc.txt <- ifelse(input$ens_download_preview_perc == 1, "perc_", "values_")
  res.txt <- ifelse(input$ens_download_preview_res == 1, "300ppi", "72ppi")
  file.ext <- switch(input$ens_download_preview_format,
                     "1" = ".jpeg", "2" = ".pdf", "3" = ".png")
  
  # Multi
  if (length(input$ens_datatable_ensembles_rows_selected) > 1) {
    f.val <- paste0("Multi_", perc.txt, res.txt, file.ext)
  }
  # Single
  else {
    idx.selected <- as.numeric(input$ens_datatable_ensembles_rows_selected)
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
                    ens.idx.txt, perc.txt, res.txt, file.ext)
  }
  
  #"C:/Ensemble Shiny/Ensemble_R_Shiny/Plots_downloaded/",
  
  textInput("ens_download_preview_name", h5("File name"), value = f.val)
})

###############################################################################
