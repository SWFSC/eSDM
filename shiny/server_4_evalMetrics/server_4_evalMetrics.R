### Calculate AUC and/or TSS of selected models given p/a points
### 'p/a' or 'pa' refers to presence/absence points


###############################################################################
# Flags and resetting of inputs and reactiveValues

### Flag for if any model predictions are in app
output$eval_display_flag <- reactive({
  list.models.all <- list(vals$models.ll, 
                          vals$overlaid.models, 
                          vals$ensemble.models)
  
  any(sapply(list.models.all, length) > 0)
})
outputOptions(output, "eval_display_flag", suspendWhenHidden = FALSE)

output$eval_display_calc_metrics_flag <- reactive({
  all(!is.na(vals$eval.data.list))
})
outputOptions(output, "eval_display_calc_metrics_flag", 
              suspendWhenHidden = FALSE)


###############################################################################
# Functions for calculating metrics

### Calculate metrics
eval_metrics <- eventReactive(input$eval_metrics_execute, {
  # Reset eval reactiveValues
  vals$eval.models.idx <- NULL
  vals$eval.metrics <- list()
  vals$eval.metrics.names <- NULL
  
  # Set necessary variables
  pa.sptsdf <- vals$eval.data.list
  pa.sptsdf.both <- suppressWarnings(all(!sapply(pa.sptsdf, is.na)))
  
  models.idx.any <- any(!sapply(eval_models_toeval_idx(), is.null))
  
  which.metrics <- input$eval_metrics_which
  
  # All validating done here so all messages are displayed at same time
  validate(
    need(pa.sptsdf.both, 
         paste("Please load presence and absence points in order", 
               "to calculate model evaluation metrics")),
    need(models.idx.any, 
         paste("Please select at least one model for which ", 
               "to calculate model evaluation metrics")),
    need(!is.null(which.metrics), 
         "Please select at least one evaluation metric to calculate")
  ) 
  
  # Calculate metrics
  withProgress(message = 'Evaluating models', value = 0.1, {
    models.toeval <- eval_models_toeval()
    incProgress(0.1)
    
    eval.results <- lapply(models.toeval, function(m) {
      helper.over.out <- helper.over(pa.sptsdf[[1]], pa.sptsdf[[2]], m, 1)
      helper.pred.out <- helper.pred(pa.sptsdf[[1]], pa.sptsdf[[2]], m, 1, 
                                     helper.over.result = helper.over.out)
      
      out1 <- out2 <- out3 <- NULL
      if("AUC" %in% which.metrics) { 
        out1 <- auc.func(pa.sptsdf[[1]], pa.sptsdf[[2]], m, 1, "", 
                         helper.pred.result = helper.pred.out)
      }
      if("TSS" %in% which.metrics) {
        out2 <- tss.func(pa.sptsdf[[1]], pa.sptsdf[[2]], m, 1, 
                         helper.pred.result = helper.pred.out)
      }
      if("RMSE" %in% which.metrics) {
        out3 <- rmse.func(pa.sptsdf[[1]], pa.sptsdf[[2]], m, 1, 
                          helper.over.result = helper.over.out)
      }
      
      incProgress(amount = 0.8 / length(models.toeval))
      
      c(out1, out2, out3)
    })
  })
  
  # Save model idx and metrics to reactiveValues
  vals$eval.models.idx <- eval_models_toeval_idx()
  vals$eval.metrics <- eval.results
  vals$eval.metrics.names <- which.metrics
  
  return("Metric(s) calculated")
})


### Generate indicies of models for which to calculate metrics
eval_models_toeval_idx <- reactive({
  eval.models.idx <- list(input$eval_models_table_orig_out_rows_selected, 
                          input$eval_models_table_over_out_rows_selected, 
                          input$eval_models_table_ens_out_rows_selected)
  
  lapply(eval.models.idx, function(i) {
    if (!is.null(i)) sort(i) else i
  })
})

### Generate list of crs.ll models with which to calculate metrics
# Validating done in eval_metrics()
eval_models_toeval <- reactive({
  eval.models.idx <- eval_models_toeval_idx()
  
  models.list.orig <- vals$models.ll[eval.models.idx[[1]]]
  models.list.over <- vals$overlaid.models[eval.models.idx[[2]]]
  models.list.ens <- vals$ensemble.models[eval.models.idx[[3]]]
  
  models.all <- c(models.list.orig, models.list.over, models.list.ens)
  models.all.ll <- lapply(models.all, function(j) {
    if (identical(crs(j), crs.ll)) {
      j
    } else {
      spTransform(j, crs.ll)
    }
  })
  
  models.all.ll
})


###############################################################################
# Evaluation metrics table

### Generate table of calculated metrics
table_eval_metrics <- reactive({
  req(length(vals$eval.metrics) > 0, vals$eval.metrics.names)
  
  metrics.table <- as.data.frame(t(as.data.frame(vals$eval.metrics)))
  names(metrics.table) <- vals$eval.metrics.names
  
  models.idx <- vals$eval.models.idx
  isolate({
    table.orig <- table_orig()[models.idx[[1]], ]
    table.over <- table_overlaid()[models.idx[[2]], ]
    table.ensembles <- table_ensembles()[models.idx[[3]], ]
  })
  
  metrics.table <- data.frame("Model" = c(row.names(table.orig), 
                                          row.names(table.over), 
                                          row.names(table.ensembles)),
                              metrics.table, 
                              stringsAsFactors = F)
  row.names(metrics.table) <- 1:nrow(metrics.table)
  
  metrics.table
})

### Download table
output$eval_metrics_table_save <- downloadHandler(
  filename = paste0("Eval_metrics.csv"),
  content = function(file) {
    # browser()
    eval.metrics <- table_eval_metrics()
    
    # Get info about selected predictions
    models.which <- vals$eval.models.idx
    orig.models <- cbind(table_orig()[models.which[[1]],], 
                         table_orig_stats()[models.which[[1]],2:6])
    over.models <- table_overlaid()[models.which[[2]],]
    # orig.over.models <- rbind(orig.models, over.models)
    ens.models <- cbind(table_ensembles()[models.which[[3]],], 
                        NA, NA, NA, NA, NA, NA)
    all.models.names <- c(paste(names(orig.models), names(ens.models), 
                                sep = "/")[1:4], 
                          names(orig.models)[5:10])
    
    names(orig.models) <- all.models.names
    names(over.models) <- all.models.names
    names(ens.models) <- all.models.names
    all.models <- rbind(orig.models, over.models, ens.models)
    
    eval.metrics.models <- cbind(eval.metrics, all.models)
    
    # eval.metrics.ens.which <- which(grepl("Ensemble", eval.metrics$Model))
    # eval.metrics.orig.over <- cbind(eval.metrics[-eval.metrics.ens.which,], 
    #                                 orig.over.models)
    # eval.metrics.ens <- cbind(eval.metrics[eval.metrics.ens.which,], ens.models)
    # write.csv(eval.metrics.ens, file = "Eval_metrics_ensemble.csv", row.names = FALSE)
    
    write.csv(eval.metrics.models, file = file, row.names = FALSE)
  }
)

###############################################################################
