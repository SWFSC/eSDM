### renderUI() functions for Create (simple) Ensemble section


###############################################################################
# 'Create Ensemble Predictions' box, weighted ensembling widgets

#----------------------------------------------------------
# Method 1

### Weights for a manually weighted ensemble
output$create_ens_weight_manual_uiOut_text <- renderUI ({
  models.num <- length(vals$overlaid.models)

  if (input$create_ens_table_subset) {
    models.num <- length(input$create_ens_datatable_rows_selected)
  }

  text.val <- paste(rep("1.0", models.num), collapse = ", ")
  textInput("create_ens_weight_manual", tags$h5("Ensemble weights"),
            value = text.val, width = "40%")
})


#----------------------------------------------------------
# Method 2

### Checkboxes with metrics that have been calculated and thus can be weights
output$create_ens_weights_metric_uiOut_radio <- renderUI({
  choice.input <- vals$eval.metrics.names

  radioButtons("create_ens_weights_metric",
               tags$h5("Metric to use for weights"),
               choices = choice.input, selected = NULL)
})


#----------------------------------------------------------
# Method 3
### None


#----------------------------------------------------------
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
                 tags$h5("Overlaid predictions to which to apply polygon weights"),
                 choices = input.val, selected = NULL, multiple = TRUE)
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
                 tags$h5("Select loaded weight polygon(s) to remove"),
                 choices = choices.list, selected = NULL, multiple = TRUE)
})


#######################################
### Widget to select overlaid predictions for which to plot preds and wpolys
output$create_ens_weights_poly_preview_model_uiOut_select <- renderUI({
  models.which <- seq_along(vals$overlaid.models)

  if (input$create_ens_table_subset) {
    ens.selected <- input$create_ens_datatable_rows_selected
    req(any(models.which %in% ens.selected))
    models.which <- models.which[models.which %in% ens.selected]
  }

  input.val <- as.list(paste("Overlaid", models.which))
  choices.list <- as.list(models.which)
  names(choices.list) <- input.val

  selectInput("create_ens_weights_poly_preview_model", NULL,
              choices = choices.list, selected = NULL)
})


#######################################
### Button to plot overlaid predictions and their weight polys
output$create_ens_weights_poly_preview_execute_uiOut_button <- renderUI({
  req(
    vals$ens.over.wpoly.filename, input$create_ens_weights_poly_preview_model
  )

  # browser()
  overlaid.which <- as.numeric(input$create_ens_weights_poly_preview_model)
  validate(
    need(isTruthy(vals$ens.over.wpoly.sf[[overlaid.which]]),
         paste("This overlaid model does not have any",
               "assigned weight polygons to preview"))
  )

  actionButton("create_ens_weights_poly_preview_execute", "Plot preview")
})


###############################################################################
# 'Create Ensemble Predictions' box, other widgets

### Widget with options for rescaling
output$create_ens_rescale_type_uiOut_radio <- renderUI({
  if (input$create_ens_table_subset) {
    models.which <- input$create_ens_datatable_rows_selected
  } else {
    models.which <- seq_along(vals$overlaid.models)
  }
  req(length(models.which) >= 2)

  pred.type <- vals$models.pred.type[models.which]

  choices.list <- list("None" = 1, "Abundance" = 2, "Normalization" = 3,
                       "Standardization" = 4, "Sum to 1" = 5)
  if (!all(pred.type == "1")) choices.list <- choices.list[2:5]

  radioButtons("create_ens_rescale_type", NULL,
               choices = choices.list, selected = choices.list[[1]])
})


### Button to create ensembles
output$create_ens_create_action_uiOut_button <- renderUI({
  if (input$create_ens_table_subset) {
    models.which <- input$create_ens_datatable_rows_selected
  } else {
    models.which <- seq_along(vals$overlaid.models)
  }
  req(length(models.which) >= 2)

  actionButton("create_ens_create_action", "Create ensemble")
})


###############################################################################
# 'Created Ensemble Predictions' box

### actionButton for interactive preview
output$ens_preview_interactive_execute_uiOut_button <- renderUI({
  req(input$ens_select_action == 1)

  validate(
    need(length(input$ens_datatable_ensembles_rows_selected) == 1,
         paste("You can only interactively preview one set of ensemble",
               "model predictions at a time")),
    errorClass = "validation2"
  )

  actionButton("ens_preview_interactive_execute",
               "Preview selected model predictions interactively")
})


### textInput with default filename for download of ensemble preview
output$ens_download_preview_name_uiOut_text <- renderUI({
  req(
    input$ens_select_action == 3, input$ens_datatable_ensembles_rows_selected
  )

  # Same for multi- and single- preview
  perc.txt <- ifelse(input$ens_download_preview_perc == 1, "perc_", "values_")
  res.txt <- ifelse(input$ens_download_preview_res == 1, "300ppi", "72ppi")
  file.ext <- switch(input$ens_download_preview_format,
                     "1" = ".jpeg", "2" = ".pdf", "3" = ".png")

  if (length(input$ens_datatable_ensembles_rows_selected) > 1) {
    # Multi
    f.val <- paste0("eSDM_multi_", perc.txt, res.txt, file.ext)

  } else {
    # Single
    idx.selected <- as.numeric(input$ens_datatable_ensembles_rows_selected)
    ens.method.txt <- switch(vals$ensemble.method[idx.selected],
                             "Unweighted" = "UnW_", "Weighted" = "W_")
    ens.weights.txt <- vals$ensemble.weights[idx.selected]
    ens.weights.txt <- ifelse(
      is.na(ens.weights.txt), "", paste0(gsub(", ", "+", ens.weights.txt), "_")
    )
    ens.rescale.txt <- vals$ensemble.rescaling[idx.selected]
    ens.rescale.txt <- ifelse(
      grepl("Abund", ens.rescale.txt),
      paste0("Abund", strsplit(ens.rescale.txt, ": ")[[1]][2], "_"),
      switch(ens.rescale.txt, "None" = "None_", "Normalization" = "Norm_",
             "Standardization" = "Stand_", "Sum to 1" = "Sumto1_")
    )
    ens.idx.txt <- vals$ensemble.overlaid.idx[idx.selected]
    ens.idx.txt <- paste0(gsub(", ", "+", ens.idx.txt), "_")

    f.val <- paste0(
      "eSDM_", ens.method.txt, ens.weights.txt, ens.rescale.txt, ens.idx.txt,
      perc.txt, res.txt, file.ext
    )
  }

  textInput("ens_download_preview_name", tags$h5("File name"), value = f.val)
})


### actionButton to calculate abundances
output$ens_calc_abund_execute_uiOut_button <- renderUI({
  ens.rows <- input$ens_datatable_ensembles_rows_selected
  req(input$ens_select_action == 5, ens.rows)

  ens.rescalings <- vals$ensemble.rescaling[ens.rows]
  rescaling.abund.bad <- c("Normalization", "Standardization", "Sum to 1")

  validate(
    need(all(!(ens.rescalings %in% rescaling.abund.bad)),
         paste("Abundance cannot be reasonably calculated for ensembles",
               "made with predictions rescaled using the 'Normalization',",
               "'Standardization', or 'Sum to 1' methods")),
    errorClass = "validation2"
  )

  actionButton("ens_calc_abund_execute", "Calculate abundance(s)")
})

###############################################################################
