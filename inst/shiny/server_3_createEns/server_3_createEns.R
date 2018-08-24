### Create a simple ensemble from overlaid models...
### ...and process ensembles once they're created


###############################################################################
# Flags for conditionalPanel

#------------------------------------------------
### Flag for if overlaid models have been created
output$ens_display_flag <- reactive({
  length(vals$overlaid.models) != 0
})
outputOptions(output, "ens_display_flag", suspendWhenHidden = FALSE)


#------------------------------------------------
### Flag for if at least 2 overlaid models are selected to ensemble
output$ens_overlaid_selected_flag <- reactive({
  length(create_ens_overlaid_idx_num()) >= 2
})
outputOptions(output, "ens_overlaid_selected_flag", suspendWhenHidden = FALSE)


#------------------------------------------------
### Flag for if 1+ ensemble predictions have been created
output$ens_display_ens_flag <- reactive({
  length(vals$ensemble.models) > 0
})
outputOptions(output, "ens_display_ens_flag", suspendWhenHidden = FALSE)


#------------------------------------------------
### Flag for if 1+ created ensemble models are selected to be used in action
output$ens_models_selected_flag <- reactive({
  isTruthy(input$ens_datatable_ensembles_rows_selected)
})
outputOptions(output, "ens_models_selected_flag", suspendWhenHidden = FALSE)


###############################################################################
# Process created ensembles

###########################################################
### Remove selected ensemble(s)
ens_remove <- eventReactive(input$ens_remove_execute, {
  idx <- input$ens_datatable_ensembles_rows_selected

  validate(
    need(length(idx) > 0,
         paste("Error: Please select one or more sets of",
               "ensemble predictions to remove"))
  )

  #------------------------------------
  ### Remove the reactiveValue info for selected set(s) of ensemble predicitons
  vals$ensemble.models <- vals$ensemble.models[-idx]
  vals$ensemble.method <- vals$ensemble.method[-idx]
  vals$ensemble.weights <- vals$ensemble.weights[-idx]
  vals$ensemble.rescaling <- vals$ensemble.rescaling[-idx]
  vals$ensemble.overlaid.idx <- vals$ensemble.overlaid.idx[-idx]

  if(length(vals$ensemble.method) == 0) vals$ensemble.method <- NULL
  if(length(vals$ensemble.weights) == 0) vals$ensemble.weights <- NULL
  if(length(vals$ensemble.rescaling) == 0) vals$ensemble.rescaling <- NULL
  if(length(vals$ensemble.overlaid.idx) == 0) vals$ensemble.overlaid.idx <- NULL


  # Handle other places this data was used
  #------------------------------------
  ### If predictions being removed were interactively previewed, remove preview
  ### Else, adjust vals idx
  if (isTruthy(vals$ensemble.plot.leaf.idx)) {
    if (isTruthy(any(idx %in% vals$ensemble.plot.leaf.idx))) {
      vals$ensemble.plot.leaf <- NULL
      vals$ensemble.plot.leaf.idx <- NULL

    } else {
      idx.adjust <- sapply(vals$ensemble.plot.leaf.idx, function(i) sum(idx < i))
      vals$ensemble.plot.leaf.idx <- vals$ensemble.plot.leaf.idx - idx.adjust
      validate(
        need(all(vals$ensemble.plot.leaf.idx > 0),
             "Error: While deleting ensemble model(s), error 1")
      )
    }
  }

  #------------------------------------
  ### If predictions being removed were staticly previewed, remove preview
  ### Else, adjust vals idx
  if (isTruthy(vals$ensemble.plot.idx)) {
    if (isTruthy(any(idx %in% vals$ensemble.plot.idx))) {
      vals$ensemble.plot <- NULL
      vals$ensemble.plot.idx <- NULL

    } else {
      idx.adjust <- sapply(vals$ensemble.plot.idx, function(i) sum(idx < i))
      vals$ensemble.plot.idx <- vals$ensemble.plot.idx - idx.adjust
      validate(
        need(all(vals$ensemble.plot.idx > 0),
             "Error: While deleting ensemble model(s), error 1b")
      )
    }
  }

  #------------------------------------
  ### Remove evaluation metrics if they're calculated for ensemble model preds
  # TODO: make this so it only removes the metrics of models being removed
  if (isTruthy(vals$eval.models.idx[[3]])) {
    vals$eval.models.idx <- NULL
    vals$eval.metrics <- NULL
    vals$eval.metrics.names <- NULL
  }

  #------------------------------------
  ""
})


###########################################################
### Calculate predicted abundance(s)
ens_abund_values <- reactive({
  validate(
    need(length(input$ens_datatable_ensembles_rows_selected) > 0,
         paste("Error: Please select at least one set of",
               "ensemble predictions from the table"))
  )

  ensemble.which <- sort(input$ens_datatable_ensembles_rows_selected)

  ens.tocalc <- vals$ensemble.models[ensemble.which]
  ens.abund <- round(sapply(ens.tocalc, eSDM::model_abundance, "Pred.ens"), 0)
  names(ens.abund) <- paste("Ensemble", ensemble.which)

  ens.abund
})

### Generate table of calculated abundances
table_ens_abund <- eventReactive(input$ens_calc_abund_execute, {
  req(input$ens_select_action == 5)
  ens.abund <- ens_abund_values()

  data.frame(
    "Predictions" = names(ens.abund), "Abundance" = unname(ens.abund),
    stringsAsFactors = FALSE
  )
})

###############################################################################
