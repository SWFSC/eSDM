### Code for loading models and converting them to sf objects


###############################################################################
### Flag for if any model predictions are loaded
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
### Show/hide interactive/static plot outputs is in 'server_hide+show.R'


###############################################################################
### Delete selected model
observeEvent(input$model_remove_execute, {
  idx <- as.numeric(input$models_loaded_table_rows_selected)
  req(length(idx) > 0)

  #########################################################
  ### Remove the reactiveValue info for selected set(s) of model predicitons
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
  # Handle relevant places this sdm was used/displayed

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
             "Error: While deleting original model(s), error 1b")
      )
    }
  }

  ### Remove evaluation metrics if they're calculated for original model preds
  # TODO: make this so it only removes the metrics of models being removed
  if (isTruthy(vals$eval.models.idx)) {
    if (!is.null(vals$eval.models.idx[[1]])){
      vals$eval.models.idx <- NULL
      vals$eval.metrics <- NULL
      vals$eval.metrics.names <- NULL
    }
  }

  ### If these predictions were pretty-plotted, reset and hide pretty plot
  ### Else, adjust vals idx
  if (isTruthy(vals$pretty.plotted.idx)) {
    browser()
    if (any(idx %in% vals$pretty.plotted.idx[[1]])) {
      shinyjs::hide("pretty_plot_plot", time = 0)
      vals$pretty.params.list <- NULL
      vals$pretty.plotted.idx <- NULL
    } else {
      idx.adjust <- sapply(vals$pretty.plotted.idx[[1]], function(i) {
        sum(idx < i)
      })
      vals$pretty.plotted.idx[[1]] <- vals$pretty.plotted.idx[[1]] - idx.adjust
      validate(
        need(all(vals$pretty.plotted.idx[[1]] > 0),
             "Error: While deleting 1+ original model(s), error 2")
      )
    }
  }
})


###############################################################################
### Reset 'Prediction value type' to 'Relative' if new file is loaded
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
### Ensure all original model reactive values are correctly formatted
observe({
  vals$models.ll
  vals$models.orig
  vals$models.names
  vals$models.data.names
  vals$models.pred.type
  vals$models.specs

  check.all <- eSDM::zero_range(
    sapply(list(vals$models.ll, vals$models.orig, vals$models.names,
                vals$models.data.names, vals$models.pred.type,
                vals$models.specs),
           length)
  )
  if (!(check.all)) shinyjs::alert(
    "eSDM error 1: Improper formatting of 'original' objects, please contact Sam"
  )

  if (length(vals$models.ll) != 0) {
    check.all <- c(
      all(sapply(vals$models.ll, inherits, "sf")),
      all(sapply(vals$models.orig, inherits, "sf")),

      all(sapply(lapply(vals$models.ll, names), function(i) identical(i, c("Pred", "Weight", "Pixels", "geometry")))),
      all(sapply(lapply(vals$models.orig, names), function(i) identical(i, c("Pred", "Weight", "Pixels", "geometry")))),

      all(sapply(lapply(vals$models.ll, function(i) names(st_agr(i))), function(j) identical(j, c("Pred", "Weight", "Pixels")))),
      all(sapply(lapply(vals$models.orig, function(i) names(st_agr(i))), function(j) identical(j, c("Pred", "Weight", "Pixels")))),

      all(sapply(vals$models.ll, function(i) identical(st_crs(i), st_crs(4326)))),

      all(sapply(vals$models.ll, st_agr) == "constant"),
      all(sapply(vals$models.orig, st_agr) == "constant"),

      all(sapply(vals$models.ll, attr, "sf_column") == "geometry"),
      all(sapply(vals$models.orig, attr, "sf_column") == "geometry"),

      sapply(lapply(vals$models.ll, function(i) i$Pred), inherits, c("numeric", "integer")),
      sapply(lapply(vals$models.ll, function(i) i$Weight), inherits, c("numeric", "integer")),
      sapply(lapply(vals$models.ll, function(i) i$Pixels), inherits, c("numeric", "integer")),

      all(sapply(vals$models.names, inherits, "character")),
      all(sapply(vals$models.data.names, function(i) all(sapply(i, inherits, "character"))))
    )

    if (!all(check.all)) {
      browser()
      shinyjs::alert(
        "eSDM error 2: Improper formatting of 'original' objects, please contact Sam"
      )
    }
  }
})

###############################################################################
