### Create a simple ensemble from overlaid models...
### ...and process ensembles once they're created


###############################################################################
# Flags for conditionalPanel

### Flag for if overlaid models have been created
output$ens_display_flag <- reactive({
  length(vals$overlaid.models) != 0
})
outputOptions(output, "ens_display_flag", suspendWhenHidden = FALSE)

### Flag for if at least 2 overlaid models are selected to ensemble
output$ens_overlaid_selected_flag <- reactive({
  length(create_ens_overlaid_idx_num()) >= 2
  # browser()
})
outputOptions(output, "ens_overlaid_selected_flag", suspendWhenHidden = FALSE)

### Flag for if all prediction types are 'Absolute'
output$ens_rescale_none_flag <- reactive({
  all(vals$models.pred.type == "1")
})
outputOptions(output, "ens_rescale_none_flag", suspendWhenHidden = FALSE)

### Flag for if evaluation metrics have been calculated
# This flag is in 'ensCreateEns_create_weighted.R'

### Flag for if 1+ ensemble predictions have been created
output$ens_display_ens_flag <- reactive({
  length(vals$ensemble.models) > 0
})
outputOptions(output, "ens_display_ens_flag", suspendWhenHidden = FALSE)


###############################################################################
# Process/plot created ensembles

###########################################################
### Generate preview of selected ensemble predictions
create_ens_preview_model <- reactive({
  ensemble.which <- as.numeric(input$ens_datatable_ensembles_rows_selected)
  
  validate(
    need(length(ensemble.which) > 0,
         paste("Error: Please select at least one set of", 
               "ensemble predictions from the table"))
  )
  ensemble.spdf <- vals$ensemble.models[ensemble.which]
  
  # Get SPixDF object with data being rasterized pixel indices
  ens.pix <- vals$ens.over.pix 
  names(ens.pix) <- "Pred.ens.pix"
  ens.pix.idx <- vals$ens.over.pix$pix
  
  ens.pix.list <- lapply(ensemble.spdf, function(i) {
    ens.pix.curr <- ens.pix
    ens.pix.curr$Pred.ens.pix <- i$Pred.ens[ens.pix.idx]
    names(ens.pix.curr) <- "Pred.ens"
    
    ens.pix.curr
  })
  
  ens.pix.list
})


###########################################################
### Remove selected ensemble(s)
ens_remove <- eventReactive(input$ens_remove_execute, {
  idx <- input$ens_datatable_ensembles_rows_selected
  
  validate(
    need(length(idx) > 0, 
         paste("Error: Please select one or more sets of", 
               "ensemble predictions to remove"))
  )
  
  ####################################
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
  
  
  ####################################
  # Handle other places this data was used
  
  ### If these predictions were previewed, hide preview
  ### Else, adjust vals idx
  if (!is.null(vals$ensemble.plotted.idx)) {
    if (any(idx %in% vals$ensemble.plotted.idx)) {
      shinyjs::hide("ens_pix_preview_plot", time = 0)
      vals$ensemble.plotted.idx <- NULL
    } else {
      idx.adjust <- sapply(vals$ensemble.plotted.idx, function(i) {
        sum(idx < i)
      })
      vals$ensemble.plotted.idx <- vals$ensemble.plotted.idx - idx.adjust
      validate(
        need(all(vals$ensemble.plotted.idx > 0), 
             "Error: While deleting ensemble model(s), error 1")
      )
    }
  }
  
  ### Remove evaluation metrics if they're calculated for ensemble model preds
  # TODO: make this so it only removes the metrics of models being removed
  if (!is.null(vals$eval.models.idx)) {
    if (!is.null(vals$eval.models.idx[[3]])) {
      vals$eval.models.idx <- NULL
      vals$eval.metrics <- list()
      vals$eval.metrics.names <- NULL
    }
  }
  
  ### If these predictions were pretty-plotted, reset and hide pretty plot
  ### Else, adjust vals idx
  if (!is.null(vals$pretty.plotted.idx)) {
    if (any(idx %in% vals$pretty.plotted.idx[[3]])) {
      shinyjs::hide("pretty_plot_plot", time = 0)
      vals$pretty.params.list <- list()
      vals$pretty.plotted.idx <- NULL
    } else {
      idx.adjust <- sapply(vals$pretty.plotted.idx[[3]], function(i) {
        sum(idx < i)
      })
      vals$pretty.plotted.idx[[3]] <- vals$pretty.plotted.idx[[3]] - idx.adjust
      validate(
        need(all(vals$pretty.plotted.idx[[3]] > 0), 
             "Error: While deleting ensemble model(s), error 2")
      )
    }
  }
  
  return("")
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
  ens.abund <- round(sapply(ens.tocalc, model.abundance, "Pred.ens"), 0)
  names(ens.abund) <- paste("Ensemble", ensemble.which)
  
  ens.abund
})

### Generate table of calculated abundances
table_ens_abund <- eventReactive(input$ens_calc_abund_execute, {
  ens.abund <- ens_abund_values()
  
  # data.frame(Predictions = names(ens.abund), Abundance = unname(ens.abund), 
  #            stringsAsFactors = FALSE)
  ens.abund.table <- as.data.frame(t(data.frame(names(ens.abund), 
                                                unname(ens.abund), 
                                                stringsAsFactors = FALSE)))
  row.names(ens.abund.table) <- c("Predictions", "Abundance")
  
  ens.abund.table
})

###############################################################################
