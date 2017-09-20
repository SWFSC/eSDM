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

#################################################
### Remove selected ensemble(s)
ens_remove <- eventReactive(input$ens_remove_execute, {
  idx <- input$ens_datatable_ensembles_rows_selected
  
  validate(
    need(length(idx) > 0, 
         "Please select one or more sets of ensemble predictions to remove")
  )
  
  # Hide preview if these predictions were plotted
  plotted.which <- model_pix_preview_event()[[2]]
  if (any(idx %in% plotted.which)) {
    shinyjs::hide("model_pix_preview_plot", time = 0)
  }
  
  browser()
  
  # Remove the reactiveValue info for selected set(s) of ensemble predicitons
  vals$ensemble.models <- vals$ensemble.models[-idx]
  vals$ensemble.method <- vals$ensemble.method[-idx]
  vals$ensemble.weights <- vals$ensemble.weights[-idx]
  vals$ensemble.rescaling <- vals$ensemble.rescaling[-idx]
  vals$ensemble.overlaid.idx <- vals$ensemble.overlaid.idx[-idx]
  
  if(length(vals$ensemble.method) == 0) vals$ensemble.method <- NULL
  if(length(vals$ensemble.weights) == 0) vals$ensemble.weights <- NULL
  if(length(vals$ensemble.rescaling) == 0) vals$ensemble.rescaling <- NULL
  if(length(vals$ensemble.overlaid.idx) == 0) vals$ensemble.overlaid.idx <- NULL
  
  return("")
})


#################################################
### Calculate predicted abundance(s)
ens_abund_values <- reactive({
  validate(
    need(length(input$ens_datatable_ensembles_rows_selected) > 0, 
         paste("Please select at least one set of ensemble predictions", 
               "from the table"))
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


#################################################
### Generate preview of selected ensemble predictions
create_ens_preview_model <- reactive({
  ensemble.which <- sort(input$ens_datatable_ensembles_rows_selected)
  
  validate(
    need(length(ensemble.which) > 0,
         "Please select at least one set of ensemble predictions from the table")
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

###############################################################################
