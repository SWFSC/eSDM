### renderUI() functions for Create Ensemble section


###############################################################################
# 'Create Ensemble Predictions' box, other widgets

create_ens_rescale_type_helper <- reactive({
  if (input$create_ens_table_subset) {
    models.which <- input$create_ens_datatable_rows_selected
  } else {
    models.which <- seq_along(vals$overlaid.models)
  }
  req(length(models.which) >= 2)

  models.which
})


### Message for if not all predictions are absolute dens/abundance
output$create_ens_rescale_type_message <- renderUI({
  models.which <- create_ens_rescale_type_helper()
  pred.type <- vals$models.pred.type[models.which]

  validate(
    need(all(pred.type %in% c("1", "3")),
         paste("The selected overlaid predictions likely need to be rescaled,",
               "as their prediction value types are not all",
               "\"Absolute density\" or \"Abundance\"")),
    errorClass = "validation2"
  )

  NULL
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
# Regional exclusion

#----------------------------------------------------------
### Widget to select overlaid predictions to which to apply uploaded polys
output$create_ens_reg_model_uiOut_selectize <- renderUI({
  models.which <- seq_along(vals$overlaid.models)

  if (input$create_ens_table_subset) {
    ens.selected <- input$create_ens_datatable_rows_selected
    models.which <- models.which[models.which %in% ens.selected]
  }

  input.val <- as.list(paste("Overlaid", models.which))
  selectizeInput("create_ens_reg_model",
                 tags$h5("Overlaid predictions to which to assign exclusion polygon(s)"),
                 choices = input.val, selected = NULL, multiple = TRUE)
})


#----------------------------------------------------------
### Select assigned exclusion polygons to remove
output$create_ens_reg_remove_choices_uiOut_select <- renderUI({
  req(!all(sapply(vals$ens.over.wpoly.filename, is.null)))

  poly.table <- create_ens_reg_table()

  choices.list.names <- apply(poly.table, 1, function(i) {
    x <- unlist(strsplit(i[2], ", "))
    y <- unlist(strsplit(i[3], ", "))

    if (length(x) != 0) paste(i[1], x, y, sep = " || ") else NULL
  })

  model.which <- which(!sapply(choices.list.names, is.null))
  poly.num <- lapply(choices.list.names[model.which], seq_along)

  choices.list <- unlist(mapply(function(i, j) {
    paste(i, j, sep = ", ")
  },
  model.which, poly.num))
  names(choices.list) <- unlist(choices.list.names)

  selectizeInput("create_ens_reg_remove_choices",
                 tags$h5("Select assigned exclusion polygon(s) to remove"),
                 choices = choices.list, selected = NULL, multiple = TRUE)
})


#----------------------------------------------------------
### Widget to select overlaid predictions for which to plot preds and wpolys
output$create_ens_reg_preview_model_uiOut_select <- renderUI({
  models.which <- seq_along(vals$overlaid.models)

  if (input$create_ens_table_subset) {
    ens.selected <- input$create_ens_datatable_rows_selected
    req(any(models.which %in% ens.selected))
    models.which <- models.which[models.which %in% ens.selected]
  }

  input.val <- as.list(paste("Overlaid", models.which))
  choices.list <- as.list(models.which)
  names(choices.list) <- input.val

  selectInput("create_ens_reg_preview_model", NULL,
              choices = choices.list, selected = NULL)
})


#----------------------------------------------------------
### Button to plot overlaid predictions and their exclusion polys
output$create_ens_reg_preview_execute_uiOut_button <- renderUI({
  req(
    vals$ens.over.wpoly.filename, input$create_ens_reg_preview_model
  )

  overlaid.which <- as.numeric(input$create_ens_reg_preview_model)
  validate(
    need(isTruthy(vals$ens.over.wpoly.sf[[overlaid.which]]),
         paste("The selected overlaid predictions do not have any",
               "assigned exclusion polygons to preview")),
    errorClass = "validation2"
  )

  actionButton("create_ens_reg_preview_execute", "Plot preview")
})


###############################################################################
# Weighted ensemble widgets

#----------------------------------------------------------
# Method 1

### Weights for a manually weighted ensemble
output$create_ens_weight_manual_uiOut_text <- renderUI({
  models.num <- length(vals$overlaid.models)

  if (input$create_ens_table_subset) {
    models.num <- length(input$create_ens_datatable_rows_selected)
  }

  text.val <- paste(rep(paste0("1/", models.num), models.num), collapse = ", ")
  textInput("create_ens_weight_manual", tags$h5("Ensemble weights"),
            value = text.val, width = "40%")
})


#----------------------------------------------------------
# Method 2

### Text for if metrics cannot be used as ensemble weights
output$create_ens_weights_metric_uiOut_text <- renderUI({
  validate(
    need(all(create_ens_overlaid_idx() %in% vals$eval.models.idx[[2]]),
         paste("You must calculate at least one metric for all selected",
               "overlaid predictions to use this weighting method.",
               "Calculate metric(s) in the 'Evaluation Metrics' tab")),
    errorClass = "validation2"
  )

  NULL
})

### Checkboxes which metrics that have been calculated and thus can be weights
output$create_ens_weights_metric_uiOut_radio <- renderUI({
  req(all(create_ens_overlaid_idx() %in% vals$eval.models.idx[[2]]))
  choice.input <- vals$eval.metrics.names

  radioButtons("create_ens_weights_metric",
               tags$h5("Weights based on selected metric"),
               choices = choice.input, selected = NULL)
})


###############################################################################
# Uncertainty type: within-model or among-model

### Message about no within-model
output$create_ens_uncertainty_text <- renderUI({
  ens.which <- create_ens_overlaid_idx()
  ens.which.var <- create_ens_weights_var_which()

  validate(
    need(all(ens.which %in% ens.which.var),
         paste("To calculate within-model uncertainty, all selected",
               "predictions must have assocaited uncertainty values")),
    errorClass = "validation2"
  )

  NULL
})

### Radio buttons
output$create_ens_create_uncertainty_uiOut_radio <- renderUI({
  ens.which <- create_ens_overlaid_idx()
  ens.which.var <- create_ens_weights_var_which()

  if (all(ens.which %in% ens.which.var)) {
    choices.list <- list("Among-model uncertainty" = 1, "Within-model uncertainty" = 2)
  } else {
    choices.list <- list("Among-model uncertainty" = 1)
  }

  radioButtons("create_ens_create_uncertainty",
               tags$h5("Calculate ensemble uncertainty using"),
               choices = choices.list, selected = NULL)
})


###############################################################################
# 'Created Ensemble Predictions' box

#------------------------------------------------------------------------------
### actionButton for interactive preview
output$ens_preview_interactive_execute_uiOut_button <- renderUI({
  req(input$ens_select_action == 1)

  validate(
    need(length(input$ens_datatable_ensembles_rows_selected) == 1,
         paste("You can only interactively preview one set of ensemble",
               "predictions at a time")),
    errorClass = "validation2"
  )

  actionButton("ens_preview_interactive_execute", "Plot interactive preview")
})


#------------------------------------------------------------------------------
### textInput with default filename for download of ensemble preview
output$ens_download_preview_name_uiOut_text <- renderUI({
  req(
    input$ens_select_action == 3, input$ens_datatable_ensembles_rows_selected
  )

  # Same for multi- and single- preview
  perc.txt <- ifelse(input$ens_download_preview_perc == 1, "perc_", "values_")
  res.txt <- ifelse(input$ens_download_preview_res == 1, "300ppi", "72ppi")

  if (length(input$ens_datatable_ensembles_rows_selected) > 1) {
    # Multi
    f.val <- paste0("eSDM_multi_", perc.txt, res.txt)

  } else {
    # Single
    idx.selected <- as.numeric(input$ens_datatable_ensembles_rows_selected)
    req(idx.selected <= length(vals$ensemble.specs))
    curr.specs <- vals$ensemble.specs[[idx.selected]]

    ens.method.txt <- switch(
      strsplit(curr.specs["ensmethod"], " - ")[[1]][1],
      "Unweighted" = "UnW_", "Weighted" = "W_"
    )
    ens.weights.txt <- curr.specs["weights"]
    ens.weights.txt <- ifelse(
      is.na(ens.weights.txt), "", paste0(gsub(", ", "+", ens.weights.txt), "_")
    )
    ens.rescale.txt <- curr.specs["res"]
    ens.rescale.txt <- ifelse(
      grepl("Abund", ens.rescale.txt),
      paste0("Abund", strsplit(ens.rescale.txt, ": ")[[1]][2], "_"),
      switch(ens.rescale.txt, "None" = "None_", "Sum to 1" = "Sumto1_")
    )
    ens.idx.txt <- ifelse(
      curr.specs["idx"] == "All overlaid",
      "all", substring(curr.specs["idx"], 10)
    )
    ens.idx.txt <- paste0("Preds", gsub(", ", "+", ens.idx.txt), "_")

    f.val <- paste0(
      "eSDM_", ens.idx.txt, ens.method.txt, ens.weights.txt, ens.rescale.txt,
      perc.txt, res.txt
    )
  }

  input.lab <- "Filename (without file extension)"
  textInput("ens_download_preview_name", tags$h5(input.lab), value = f.val)
})


### Download static preview
output$ens_download_preview_execute_uiOut_download <- renderUI({
  if (input$ens_download_preview_dim == 2) {
    validate(
      need(isTruthy(session$clientData$output_ens_preview_plot_width) &&
             isTruthy(session$clientData$output_ens_preview_plot_height),
           paste("You must plot a static preview before downloading a file",
                 "with these dimensions")),
      errorClass = "validation2"
    )
  }

  idx.selected <- as.numeric(req(input$ens_datatable_ensembles_rows_selected))
  for (i in idx.selected) {
    download_check(
      vals$ensemble.models[[i]]$Pred_ens, input$ens_download_preview_perc
    )
  }

  downloadButton("ens_download_preview_execute", "Download")
})


#------------------------------------------------------------------------------
### Flag for if appropriate ensembles are selected for abundance calc
abund_reac_flag <- reactive({
  ens.rows <- input$ens_datatable_ensembles_rows_selected
  req(input$ens_select_action == 5, ens.rows)

  ens.rescalings <- vapply(
    vals$ensemble.specs[ens.rows], function(i) i["res"], "1"
  )

  !any(ens.rescalings == "Sum to 1")
})

### Text saying abundance can't be calculated
output$ens_calc_abund_execute_uiOut_text <- renderUI({
  validate(
    need(abund_reac_flag(),
         paste("Abundance cannot be reasonably calculated for ensembles",
               "made with predictions rescaled using the 'Sum to 1' method")),
    errorClass = "validation2"
  )

  NULL
})

### actionButton to calculate abundances
output$ens_calc_abund_execute_uiOut_button <- renderUI({
  req(abund_reac_flag())
  actionButton("ens_calc_abund_execute", "Calculate abundance(s)")
})

###############################################################################
