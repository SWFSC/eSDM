### Reactive functions that return display tables for various tabs


###############################################################################
### Table of original models
table_orig <- reactive({ 
  if(is.null(vals$models.names)) return()
  table.out <- data.frame(x = vals$models.names, 
                          t(as.data.frame(vals$models.data.names)),
                          vals$models.pred.type, 
                          stringsAsFactors = FALSE)
  
  table.out[,5] <- sapply(table.out[,5], function(i) {
    switch(as.character(i), "1" = "Absolute", "2" = "Relative")}
  )
  
  names(table.out) <- c("Model filename", "Prediction", "Error", "Weight",
                        "Prediction type")
  row.names(table.out) <- paste("Original", 1:nrow(table.out))
  
  table.out
})


###############################################################################
### Table of original models with stats
table_orig_stats <- reactive({
  table.out <- data.frame(x = vals$models.names, 
                          t(as.data.frame(vals$models.specs)), 
                          stringsAsFactors = FALSE)
  
  names(table.out) <- c("Model filename", "Resolution", "Cell count", 
                        "Prediction count", "Abundance", "Long, lat range")
  row.names(table.out) <- paste("Original", 1:nrow(table.out))
  
  table.out
})


###############################################################################
### Table of overlaid models
table_overlaid <- reactive({
  if(length(vals$overlaid.models) == 0) return()
  req(vals$overlay.base.idx)
  
  base.idx <- vals$overlay.base.idx
  base.name <- vals$models.names[base.idx]
  
  table.out1 <- table_orig()
  overlaid.models.specs <- t(as.data.frame(vals$overlaid.models.specs))
  
  table.out <- data.frame(table.out1, overlaid.models.specs, 
                          stringsAsFactors = FALSE)
  
  names(table.out) <- c("Model filename", "Prediction", "Error", "Weight", 
                        "Prediction type", "Resolution", "Cell count", 
                        "Prediction count", "Abundance", "Long, lat range")
  row.names(table.out) <- paste("Overlaid", 1:nrow(table.out))
  
  table.out[is.na(table.out)] <- ""
  
  table.out
})


###############################################################################
### Table of created ensemble models
table_ensembles <- reactive({
  ens.models <- vals$ensemble.models
  if(length(ens.models) == 0) return()
  
  ens.overlaid.idx <- vals$ensemble.overlaid.idx
  ens.method <- vals$ensemble.method
  ens.weights <- vals$ensemble.weights
  ens.rescaling <- vals$ensemble.rescaling
  
  table.out <- data.frame(ens.method, ens.weights, 
                          ens.rescaling, ens.overlaid.idx,  
                          stringsAsFactors = FALSE)
  
  names(table.out) <- c("Ensembling method", "Weights", 
                        "Rescaling method", "Overlaid models used")
  row.names(table.out) <- paste("Ensemble", 1:nrow(table.out))
  
  table.out
})


###############################################################################
### Table of orig and overlaid spdfs
# table_orig_overlaid <- reactive({
#   rbind(table_orig(), table_overlaid()[,1:5])
# })


###############################################################################
### Table of model metrics is made in 'ensEvalMetrics.R'

### Table of presence/absence points is made in 'ensEvalMetrics_loadData.R'
