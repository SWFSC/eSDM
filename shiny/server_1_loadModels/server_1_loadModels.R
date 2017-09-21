### Code for loading models and converting them to SPDFs


###############################################################################
### Delete selected model
observeEvent(input$model_remove_execute, {
  idx <- as.numeric(input$models_loaded_table_rows_selected)
  
  validate(
    need(length(idx) > 0, 
         "Please select one or more sets of model predictions to remove")
  )
  
  ### Hide preview if these predictions were plotted
  plotted.which <- model_pix_preview_event()[[2]]
  if (any(idx %in% plotted.which)) {
    shinyjs::hide("model_pix_preview_plot", time = 0)
  }
  
  ### Remove the reactiveValue info for selected set(s) of model predicitons
  vals$models.ll <- vals$models.ll[-idx]
  vals$models.orig <- vals$models.orig[-idx]
  vals$models.pix <- vals$models.pix[-idx]
  vals$models.names <- vals$models.names[-idx]
  vals$models.data.names <- vals$models.data.names[-idx]
  vals$models.pred.type <- vals$models.pred.type[-idx]
  vals$models.specs <- vals$models.specs[-idx]
  
  if (length(vals$models.names) == 0) vals$models.names <- NULL
  if (length(vals$models.pred.type) == 0) vals$models.pred.type <- NULL
  
  # Could make this so it only removes ensemble metrics
  # TODO: make smarter
  if (!is.null(vals$eval.models.idx)) {
    if (!is.null(vals$eval.models.idx[[1]])){
      vals$eval.models.idx <- NULL
      vals$eval.metrics <- list()
      vals$eval.metrics.names <- NULL
    }
  }
  
  # Reset pretty params only if an original model was plotted
  # TODO: make smarter
  if (length(vals$pretty.params.list) != 0) {
    if (!is.null(vals$pretty.params.list$model.idx[[1]])) {
      vals$pretty.params.list <- list()
    }
  }
})


###############################################################################
# Reset 'Prediction value type' to 'Relative' if new file is loaded

observe({
  input$model_csv_file
  updateSelectInput(session, "model_csv_pred_type", selected = 2)
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