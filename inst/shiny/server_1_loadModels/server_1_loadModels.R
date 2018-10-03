### Code for importing and converting them to sf objects
# Note 'importing predictions' was 'loading models' when code was first written


###############################################################################
### Flag for if any predictions are imported
output$loadModels_display_flag <- reactive({
  length(vals$models.ll) > 0
})
outputOptions(output, "loadModels_display_flag", suspendWhenHidden = FALSE)

### Flag for if any model predictions are selected in the table
output$loaded_models_selected_flag <- reactive({
  isTruthy(input$models_loaded_table_rows_selected)
})
outputOptions(output, "loaded_models_selected_flag", suspendWhenHidden = FALSE)


###############################################################################
### Delete selected predictions
model_remove <- eventReactive(input$model_remove_execute, {
  idx <- as.numeric(input$models_loaded_table_rows_selected)
  req(length(idx) > 0)

  #########################################################
  ### Remove the reactiveValue info for selected set(s) of predicitons
  vals$models.ll <- vals$models.ll[-idx]
  vals$models.orig <- vals$models.orig[-idx]
  vals$models.names <- vals$models.names[-idx]
  vals$models.data.names <- vals$models.data.names[-idx]
  vals$models.pred.type <- vals$models.pred.type[-idx]
  vals$models.specs <- vals$models.specs[-idx]

  if (length(vals$models.names) == 0) vals$models.names <- NULL
  if (length(vals$models.data.names) == 0) vals$models.data.names <- NULL
  if (length(vals$models.pred.type) == 0) vals$models.pred.type <- NULL
  if (length(vals$models.specs) == 0) vals$models.specs <- NULL


  #########################################################
  # Handle relevant places these preds was used/displayed

  ### If these predictions were interactively previewed, remove preview
  ###   Else, adjust vals idx
  if (isTruthy(vals$models.plot.leaf.idx)) {
    if (isTruthy(any(idx %in% vals$models.plot.leaf.idx))) {
      vals$models.plot.leaf <- NULL
      vals$models.plot.leaf.idx <- NULL

    } else {
      idx.adjust <- sapply(vals$models.plot.leaf.idx, function(i) sum(idx < i))
      vals$models.plot.leaf.idx <- vals$models.plot.leaf.idx - idx.adjust
      validate(
        need(all(vals$models.plot.leaf.idx > 0),
             "Error: While deleting original model(s), error 1")
      )
    }
  }

  ### If these predictions were staticly previewed, remove preview
  ###   Else, adjust vals idx
  if (isTruthy(vals$models.plot.idx)) {
    if (isTruthy(any(idx %in% vals$models.plot.idx))) {
      vals$models.plot <- NULL
      vals$models.plot.idx <- NULL

    } else {
      idx.adjust <- sapply(vals$models.plot.idx, function(i) sum(idx < i))
      vals$models.plot.idx <- vals$models.plot.idx - idx.adjust
      validate(
        need(all(vals$models.plot.idx > 0),
             "Error: While deleting original predictions(s), error 1b")
      )
    }
  }

  ### Remove evaluation metrics if they're calculated for original preds
  # TODO: make this so it only removes the metrics of preds being removed
  if (isTruthy(vals$eval.models.idx)) {
    if (!is.null(vals$eval.models.idx[[1]])){
      vals$eval.models.idx <- NULL
      vals$eval.metrics <- NULL
      vals$eval.metrics.names <- NULL
    }
  }

  ""
})


###############################################################################
### Reset 'Prediction value type' to 'Relative' if new file is uploaded
observe({
  input$model_csv_file
  updateSelectInput(session, "model_csv_pred_type", selected = 2)
  updateSelectInput(session, "model_csv_pt_loc", selected = 1)
})
observe({
  input$model_gis_raster_file
  updateSelectInput(session, "model_gis_raster_pred_type", selected = 2)
})
observe({
  input$model_gis_shp_files
  updateSelectInput(session, "model_gis_shp_pred_type", selected = 2)
})
observe({
  input$model_gis_gdb_load
  updateSelectInput(session, "model_gis_gdb_pred_type", selected = 2)
})

###############################################################################
