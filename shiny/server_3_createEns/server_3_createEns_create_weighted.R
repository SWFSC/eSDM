### Code for creating weighted ensembles


###############################################################################
# Weighted ensembling method 1: 'Weight all model predictions by manual entry'

### Create weighted ensemble from manually entered weights
create_ens_weighted_manual <- reactive({
  overlaid.data <- create_ens_data()
  base.sp <- vals$overlay.base.sp
  weights <- create_ens_weights_num()
  
  data.ens <- data.frame(Pred.ens = apply(overlaid.data, 1, function(p) 
    weighted.mean(p, weights, na.rm = TRUE)))
  
  SpatialPolygonsDataFrame(base.sp, data.ens, match.ID = FALSE)
})

### Process text inputs for model weights
create_ens_weights_num <- reactive({
  models.weights.text <- input$create_ens_weight_manual
  models.weights <- as.numeric(unlist(strsplit(models.weights.text, ", ")))
  
  models.num <- length(vals$overlaid.models)
  if (input$create_ens_table_subset) 
    models.num <- length(input$create_ens_datatable_rows_selected)
  
  # Validate weights input
  validate(
    need(length(models.weights) == models.num, 
         paste("The number of provided weights does not", 
               "match the number of overlaid models"))
  )
  
  models.weights
})


###############################################################################
# Weighted ensembling method 2: 'Weight all model predictions by metric'

### Flag for if metrics have been calculated for selected overlaid model preds
output$create_ens_weights_metric_flag <- reactive({
  ens.overlaid.which <- create_ens_overlaid_idx()
  eval.overlaid.which <- vals$eval.models.idx[[2]]
  
  all(ens.overlaid.which %in% eval.overlaid.which)
})
outputOptions(output, "create_ens_weights_metric_flag", 
              suspendWhenHidden = FALSE)

### Table of selected metrics
create_ens_weights_metric_table <- reactive({
  req(input$create_ens_weights_metric)
  
  # Get desired metric for overlaid models from eval metrics table
  eval.metrics <- table_eval_metrics()
  idx.col <- which(names(eval.metrics) == input$create_ens_weights_metric)
  idx.row <- grep("Overlaid", eval.metrics$Model)
  weights.table <- eval.metrics[idx.row ,c(1, idx.col)]
  
  # Prep for display
  weights.table$R.weights <- weights.table[,2] / max(weights.table[,2])
  names(weights.table)[3] <- "Relative weights"
  row.names(weights.table) <- 1:nrow(weights.table)
  
  weights.table
})

### Create weighted ensemble using some evaluation metric as weights
create_ens_weighted_metric <- reactive({
  overlaid.data <- create_ens_data()
  base.sp <- vals$overlay.base.sp
  weights <- create_ens_weights_metric_table()[,2]
  
  data.ens <- data.frame(Pred.ens = apply(overlaid.data, 1, function(p) 
    weighted.mean(p, weights, na.rm = TRUE)))
  
  SpatialPolygonsDataFrame(base.sp, data.ens, match.ID = FALSE)
})


###############################################################################
# Weighted ensembling method 3: 'Use loaded spatial weights'

### Vector of idx of selected overlaid models that have spatial weights
create_ens_weights_pix_which <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.overlaid <- vals$overlaid.models[ens.which]
  ens.which.spatial <- sapply(ens.overlaid, function(i) {
    any(!is.na(i$Weight.overlaid))
  })
  
  ens.which[ens.which.spatial]
})

### Flag for if at least 1 of the selected overlaid models have spatial weights
output$create_ens_weights_pix_flag <- reactive({
  length(create_ens_weights_pix_which()) >= 1
})
outputOptions(output, "create_ens_weights_pix_flag", 
              suspendWhenHidden = FALSE)

### Table of selected overlaid models and if they have any spatial weights
create_ens_weights_pix_table <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.which.spatial <- create_ens_weights_pix_which()
  
  ens.which.spatial.text <- ifelse(ens.which %in% ens.which.spatial, 
                                   "Yes", "No")
  
  table.out <- data.frame(paste("Overlaid", ens.which), ens.which.spatial.text)
  names(table.out) <- c("Model", "Has spatial weights")
  
  table.out
})

### Generate data frame of pixel weights
create_ens_weights_pix_weights <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.which.spatial <- create_ens_weights_pix_which()
  
  df.out <- as.data.frame(lapply(ens.which, function(idx) {
    overlaid.curr <- vals$overlaid.models[[idx]]
    
    if (idx %in% ens.which.spatial) {
      validate(
        need(identical(is.na(overlaid.curr$Pred.overlaid), 
                       is.na(overlaid.curr$Weight.overlaid)), 
             paste("is.na() of Pred is different from Weight for overlaid #", 
                   idx))
      )
      overlaid.curr$Weight.overlaid
    } else {
      rep(1, length(overlaid.curr))
    }
  }))
  names(df.out) <- letters[1:ncol(df.out)]
  
  df.out
})

### Create weighted ensemble using pixel-level spatial weights
create_ens_weighted_pix <- reactive({
  overlaid.data <- create_ens_data()
  base.sp <- vals$overlay.base.sp
  weights <- create_ens_weights_pix_weights()
  
  overlaid.data.w <- overlaid.data * weights
  data.ens <- data.frame(apply(overlaid.data.w, 1, mean, na.rm = TRUE))
  names(data.ens) <- "Pred.ens"
  
  SpatialPolygonsDataFrame(base.sp, data.ens, match.ID = FALSE)
})


###############################################################################
# Weighted ensembling method 4: 'Load GIS polygon(s) with weights'

### Code is in ensCreateEns_create_weighted_poly.R

###############################################################################
