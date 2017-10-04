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

### Flag for if validation data has been loaded
output$eval_display_calc_metrics_flag <- reactive({
  all(!is.na(vals$eval.data.list))
})
outputOptions(output, "eval_display_calc_metrics_flag", 
              suspendWhenHidden = FALSE)


###############################################################################
### Generate table with validation data stats
table_data_pts <- reactive({
  data.list <- vals$eval.data.list
  data.type <- vals$eval.data.specs
  req(all(!is.na(data.list)), data.type)
  
  pres.data <- data.list[[1]]
  
  pres.num <- length(pres.data)
  abs.num <- length(data.list[[2]])
  
  if (data.type == 1) {
    count.range <- paste(range(round(pres.data$pa.num, 2)), collapse = " to ")
    table.out.col1<- c("Number of points with non-zero counts", 
                       "Number of points with counts of 0", 
                       "Range of non-zero counts")
    table.out.col2 <- c(pres.num, abs.num, count.range)
    table.out <- data.frame(x = table.out.col1, y = table.out.col2)
    
  } else if (data.type == 2) {
    table.out.col1<- c("Number of presence points", "Number of absence points")
    table.out.col2 <- c(pres.num, abs.num)
    table.out <- data.frame(x = table.out.col1, y = table.out.col2)
    
  } else {
    table.out <- NULL
  }
  
  validate(
    need(!is.null(table.out), 
         "Error: vals$eval.data.specs is not 1 or 2")
  )
  
  table.out
})


###############################################################################
# Functions for calculating metrics

###########################################################
# Prep

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


###########################################################
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
         paste("Error: Please load presence and absence points in order", 
               "to calculate model evaluation metrics")),
    need(models.idx.any, 
         paste("Error: Please select at least one model for which ", 
               "to calculate model evaluation metrics")),
    need(!is.null(which.metrics), 
         "Error: Please select at least one evaluation metric to calculate")
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


###############################################################################
# Evaluation metrics table: generate/download

###########################################################
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
                              metrics.table, stringsAsFactors = FALSE)
  row.names(metrics.table) <- 1:nrow(metrics.table)
  
  metrics.table
})


###########################################################
### Download table
# Currently set for no Error column: #'[, -3]' is to remove Error column
output$eval_metrics_table_save <- downloadHandler(
  filename = paste0("eSDM_eval_metrics.csv"),
  
  content = function(file) {
    eval.metrics <- table_eval_metrics()
    models.which <- vals$eval.models.idx
    
    ### Get info of models that have eval metrics calculated for them
    orig.table <- cbind(table_orig(), table_orig_stats()[, -1])
    orig.table <- orig.table[models.which[[1]], -3]
    over.table <- table_overlaid()[models.which[[2]], -3]
    ens.table  <- table_ensembles()[models.which[[3]], ]
    
    if (!is.null(orig.table)) {if (nrow(orig.table) == 0) orig.table <- NULL}
    if (!is.null(over.table)) {if (nrow(over.table) == 0) over.table <- NULL}
    if (!is.null(ens.table))  {if (nrow(ens.table) == 0)  ens.table <- NULL}
    
    ### Create and set column names as necessary
    if ((!is.null(orig.table) | !is.null(over.table)) & !is.null(ens.table)) {
      if (!is.null(orig.table)) {
        all.models.names <- c(paste(names(orig.table), names(ens.table), 
                                    sep = "/")[1:4], 
                              names(orig.table)[5:9])
        
        names(orig.table) <- all.models.names
      }
      if (!is.null(over.table)) {
        all.models.names <- c(paste(names(over.table), names(ens.table), 
                                    sep = "/")[1:4], 
                              names(over.table)[5:9])
        
        names(over.table) <- all.models.names
      }
      
      ens.table <- cbind(ens.table, NA, NA, NA, NA, NA)
      names(ens.table) <- all.models.names
    }
    # Else: Either some combo of orig.table and over.table or it's only
    #        the ens.table. Thus, names are already correct.
    
    
    ### Combine info tables
    models.list.all <- list(orig.table, over.table, ens.table)
    models.list.all <- models.list.all[!sapply(models.list.all, is.null)]
    
    need.check.table <- all(sapply(models.list.all, function(j, names.1) {
      names(j) == names(models.list.all[[1]])
    }, names.1 = models.list.all[[1]]))
    validate(
      need(zero_range(sapply(models.list.all, ncol)), 
           paste("Error: while downloading metrics, data tables", 
                 "info tables have different numbers of columns")), 
      need(need.check.table, 
           paste("Error: while downloading metrics, data tables", 
                 "info tables have different names"))
    )
    all.models.info <- do.call(rbind, models.list.all)
    
    
    ### Combine metric table and info table
    validate(
      need(nrow(all.models.info) == nrow(eval.metrics), 
           paste("Error: while downloading table, metrics and model", 
                 "info tables have different numbers of rows"))
    )
    eval.metrics.models.info <- cbind(eval.metrics, all.models.info)
    
    
    ### Write csv
    write.csv(eval.metrics.models.info, file = file, row.names = FALSE)
  }
)

###############################################################################
