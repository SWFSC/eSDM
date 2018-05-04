### Code for creating ensemble predictions from overlaid model predictions


###############################################################################
### Top-level function for creating ensemble
create_ensemble <- eventReactive(input$create_ens_create_action, {
  # For the validation message if num of orig models != num of overlaid models
  table_overlaid()
  
  withProgress(message = "Creating ensemble", value = 0.6, {
    ### Create ensemble
    ens.type <- input$create_ens_type
    # Unweighted
    if (ens.type == "1") ens.spdf <- create_ens_unweighted()
    
    # Weighted
    if (ens.type == "2") {
      ens.spdf <- switch(input$create_ens_weight_type, 
                         "1" = create_ens_weighted_manual(), 
                         "2" = create_ens_weighted_metric(), 
                         "3" = create_ens_weighted_pix(), 
                         "4" = create_ens_weighted_poly(), 
                         "5" = create_ens_weighted_spatial_manual())
    }
    incProgress(0.3)
    
    ### Add data to reactive variables
    vals$ensemble.models <- c(vals$ensemble.models, ens.spdf)
    vals$ensemble.overlaid.idx <- c(vals$ensemble.overlaid.idx, 
                                    create_ens_info_overlaid_idx())
    vals$ensemble.method <- c(vals$ensemble.method, 
                              create_ens_info_weighting())
    vals$ensemble.weights <- c(vals$ensemble.weights, 
                               create_ens_info_weights())
    vals$ensemble.rescaling <- c(vals$ensemble.rescaling, 
                                 create_ens_info_rescaling())
    
    
    ### Create text message to print
    text.toreturn <- paste(create_ens_info_weighting(), "ensemble")
    text.toreturn <- paste0("Created ", tolower(substring(text.toreturn, 1, 1)),
                            substring(text.toreturn, 2))
    text.toreturn <- paste0("<b>", create_ens_info_rescaling_message(), 
                            "<br/>", "<br/>", text.toreturn)
    incProgress(0.1)
  })
  
  return(text.toreturn)
})


###############################################################################
# Functions that return strings with ensemble model info for reactive vars

### Indices of overlaid models used in ensemble
create_ens_info_overlaid_idx <- reactive({
  overlaid.idx <- create_ens_overlaid_idx()
  
  if (length(overlaid.idx) == length(vals$overlaid.models)) {
    return("All")
  } else {
    return(paste(overlaid.idx, collapse = ", "))
  }
})

### Weighting method used
create_ens_info_weighting <- reactive({
  ifelse(input$create_ens_type == "1", "Unweighted", "Weighted")
})

### Weights used, if applicable
# Get weights from functions that returns numerical weights for computation
create_ens_info_weights <- reactive({
  if (input$create_ens_type == "1") {
    NA
  } else {
    switch(input$create_ens_weight_type, 
           "1" = input$create_ens_weight_manual, 
           "2" = paste(round(create_ens_weights_metric_table()[,3], 3), 
                       collapse = ", "), 
           "3" = "Spatial by pixel", 
           "4" = "Spatial by polygon")
  }
})

### Rescaling method used
create_ens_info_rescaling <- reactive ({
  input.rescale <- input$create_ens_rescale_type
  
  switch(input.rescale,
         "1" = "None",
         "2" = paste("Abundance:", input$create_ens_rescale_abund),
         "3" = "Normalization",
         "4" = "Standardization", 
         "5" = "Sum to 1")
})

### Generate string for text about created ensemble
create_ens_info_rescaling_message <- reactive({
  input.rescale <- input$create_ens_rescale_type
  
  str.rescale <- switch(input.rescale,
                        "1" = "Predictions not rescaled",
                        "2" = "abundance",
                        "3" = "normalization",
                        "4" = "standardization", 
                        "5" = "sum to 1")
  if (input.rescale != "1") {
    str.rescale <- paste("Predictions rescaled using", str.rescale, "method")
  }
  
  str.rescale
})


###############################################################################
### 'Level 1' functions - create ensemble

# Create unweighted ensemble
create_ens_unweighted <- reactive({
  overlaid.data <- create_ens_data()
  base.sp <- vals$overlay.base.sp
  
  data.ens <- data.frame(apply(overlaid.data, 1, mean, na.rm = TRUE))
  names(data.ens) <- "Pred.ens"
  
  SpatialPolygonsDataFrame(base.sp, data.ens, match.ID = FALSE)
})

####################################################################
# Create weighted ensemble: done in 'ensCreateEns_create_weighted.R'
####################################################################


###############################################################################
# 'Level 2' functions

### Get predictions to be used in ensemble
create_ens_data <- reactive({
  switch(input$create_ens_rescale_type,
         "1" = create_ens_data_extract(),        # No rescaling
         "2" = create_ens_data_rescale_abund(),  # Rescale densities to given abundance
         "3" = create_ens_data_rescale_norm(),   # Normalize densities
         "4" = create_ens_data_rescale_std(),    # Standardize densities
         "5" = create_ens_data_rescale_sumto1()) # Rescale densities so they sum to 1
})


###############################################################################
# 'Level 3' functions

#################################################
# Rescale model predictions

### Rescale model predictions by abundance
create_ens_data_rescale_abund <- reactive({
  abund <- input$create_ens_rescale_abund
  spdfs.to.rescale <- vals$overlaid.models
  
  validate(
    need(abund > 0,  
         paste("Error: Abundance must be greater", 
               "than 0 to rescale predictions"))
  )
  
  if (input$create_ens_table_subset) {
    models.which <- input$create_ens_datatable_rows_selected
    models.which <- models.which[order(models.which)]
    spdfs.to.rescale <- spdfs.to.rescale[models.which]
  }
  
  spdfs.rescaled <- models.rescale(spdfs.to.rescale, abund)
  as.data.frame(lapply(spdfs.rescaled, function(i) i$Pred.overlaid))
})

### Normalize model predictions (densities)
create_ens_data_rescale_norm <- reactive({
  data.to.rescale <- create_ens_data_extract()
  as.data.frame(apply(data.to.rescale, 2, normalize))
})

### Standardize model predictions (densities)
create_ens_data_rescale_std <- reactive({
  data.to.rescale <- create_ens_data_extract()
  as.data.frame(apply(data.to.rescale, 2, scale))
})

### Rescale model predictions (densities) to sum to 1
create_ens_data_rescale_sumto1 <- reactive({
  data.to.rescale <- create_ens_data_extract()
  as.data.frame(apply(data.to.rescale, 2, 
                      function(i) i / sum(i, na.rm = TRUE)))
})


#################################################
### Extract prediction data from overlaid models to use
create_ens_data_extract <- reactive({
  overlaid.models <- vals$overlaid.models[create_ens_overlaid_idx()]
  
  data.extracted <- as.data.frame(lapply(overlaid.models, 
                                         function(i) i$Pred.overlaid))
  names(data.extracted) <- letters[1:ncol(data.extracted)]
  
  data.extracted
})


#################################################
### Get indices of overlaid models to be used in ensemble
create_ens_overlaid_idx_num <- reactive({
  req(length(vals$overlaid.models) > 0)
  
  if (input$create_ens_table_subset) {
    if (is.null(input$create_ens_datatable_rows_selected)) {
      ens.models.which <- NULL
    } else {
      ens.models.which <- sort(input$create_ens_datatable_rows_selected)
    }
  } else {
    ens.models.which <- seq_along(vals$overlaid.models)
  }
  
  ens.models.which
})

### Get indices of overlaid models to be used in ensemble, and validate >= 2
create_ens_overlaid_idx <- reactive({
  req(length(vals$overlaid.models) > 0)
  
  ens.models.which <- create_ens_overlaid_idx_num()
  
  validate(
    need(length(ens.models.which) >= 2, 
         paste("Error: Please choose at least two sets of", 
               "model predictions for the ensemble"))
  )
  
  ens.models.which
})

###############################################################################
