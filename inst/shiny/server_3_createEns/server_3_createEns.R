# Create a simple ensemble from overlaid predictions and process ensembles
#   once they're created


###############################################################################
# Flags for conditionalPanel

#------------------------------------------------
### Flag for if overlaid predictions have been created
output$ens_display_flag <- reactive({
  length(vals$overlaid.models) > 0
})
outputOptions(output, "ens_display_flag", suspendWhenHidden = FALSE)


#------------------------------------------------
### Flag for if at least 2 overlaid predictions are selected to ensemble
output$ens_overlaid_selected_flag <- reactive({
  length(create_ens_overlaid_idx_num()) >= 2
})
outputOptions(output, "ens_overlaid_selected_flag", suspendWhenHidden = FALSE)


#------------------------------------------------
### Flag: any ensemble predictions created
output$ens_display_ens_flag <- reactive({
  length(vals$ensemble.models) > 0
})
outputOptions(output, "ens_display_ens_flag", suspendWhenHidden = FALSE)


#------------------------------------------------
### Flag: any created ensemble predictions selected to be used in action
output$ens_models_selected_flag <- reactive({
  isTruthy(input$ens_datatable_ensembles_rows_selected)
})
outputOptions(output, "ens_models_selected_flag", suspendWhenHidden = FALSE)


###############################################################################
# Remove selected ensemble(s)
ens_remove <- eventReactive(input$ens_remove_execute, {
  idx <- input$ens_datatable_ensembles_rows_selected

  validate(
    need(length(idx) > 0,
         paste("Error: Please select one or more sets of",
               "ensemble predictions to remove"))
  )

  #------------------------------------
  ### Remove the reactiveValue info for selected set(s) of ensemble predicitons
  vals$ensemble.models       <- vals$ensemble.models[-idx]
  vals$ensemble.overlaid.res <- vals$ensemble.overlaid.res[-idx]
  vals$ensemble.specs        <- vals$ensemble.specs[-idx]

  if(length(vals$ensemble.overlaid.res) == 0) vals$ensemble.overlaid.res <- NULL
  if(length(vals$ensemble.specs) == 0) vals$ensemble.specs <- NULL


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
             paste("Error: While deleting ensemble predictions, error 1.",
                   "Please report this as an issue"))
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
             paste("Error: While deleting ensemble predictions, error 1b.",
                   "Please report this as an issue"))
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


###############################################################################
# Predicted abundance

#----------------------------------------------------------
### Calculate predicted abundance(s)
ens_abund_values <- reactive({
  validate(
    need(length(input$ens_datatable_ensembles_rows_selected) > 0,
         paste("Error: Please select at least one set of",
               "ensemble predictions from the table"))
  )

  ensemble.which <- sort(input$ens_datatable_ensembles_rows_selected)

  ens.tocalc <- vals$ensemble.models[ensemble.which]
  ens.abund <- vapply(ens.tocalc, function(i) {
    round(eSDM::model_abundance(
      st_sf(i, vals$overlay.base.sfc, agr = "constant"), "Pred_ens"
    ), 1)
  }, 1)
  names(ens.abund) <- paste("Ensemble", ensemble.which)

  ens.abund
})

#----------------------------------------------------------
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
