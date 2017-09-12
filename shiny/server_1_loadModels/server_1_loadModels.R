### Code for loading models and converting them to SPDFs


###############################################################################
### Delete selected model
observeEvent(input$model_remove_execute, {
  idx <- as.numeric(input$models_loaded_table_rows_selected)

  vals$models.ll <- vals$models.ll[-idx]
  vals$models.orig <- vals$models.orig[-idx]
  vals$models.pix <- vals$models.pix[-idx]
  vals$models.names <- vals$models.names[-idx]
  vals$models.data.names <- vals$models.data.names[-idx]
  vals$models.pred.type <- vals$models.pred.type[-idx]
  vals$models.specs <- vals$models.specs[-idx]
  
  if(length(vals$models.names) == 0) vals$models.names <- NULL
  if(length(vals$models.pred.type) == 0) vals$models.pred.type <- NULL

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