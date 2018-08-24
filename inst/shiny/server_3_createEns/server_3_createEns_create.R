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
    if (ens.type == "1") ens.sf <- create_ens_unweighted()

    # Weighted
    if (ens.type == "2") {
      ens.sf <- switch(input$create_ens_weight_type,
                       "1" = create_ens_weighted_manual(),
                       "2" = create_ens_weighted_metric(),
                       "3" = create_ens_weighted_pix(),
                       "4" = create_ens_weighted_poly())
    }
    incProgress(0.3)

    ### Add data to reactive variables
    vals$ensemble.models <- c(vals$ensemble.models, list(ens.sf))
    vals$ensemble.overlaid.idx <- c(
      vals$ensemble.overlaid.idx, create_ens_info_overlaid_idx())
    vals$ensemble.method <- c(
      vals$ensemble.method, create_ens_info_weighting())
    vals$ensemble.weights <- c(
      vals$ensemble.weights, create_ens_info_weights())
    vals$ensemble.rescaling <- c(
      vals$ensemble.rescaling, create_ens_info_rescaling())
    incProgress(0.1)
  })

  paste0(
    create_ens_info_rescaling_message(),
    tags$br(), tags$br(),
    paste("Created", tolower(create_ens_info_weighting()), "ensemble")
  )
})


###############################################################################
### 'Level 1' functions - create ensemble

# Create unweighted ensemble
create_ens_unweighted <- reactive({
  overlaid.data <- create_ens_data_reg()

  st_sf(
    data.frame(Pred.ens = apply(overlaid.data, 1, mean, na.rm = TRUE)),
    geometry = vals$overlay.base.sfc, agr = "constant"
  )
})

####################################################################
# Create weighted ensemble: done in 'ensCreateEns_create_weighted.R'
####################################################################


###############################################################################
# 'Level 2' functions

### Apply regional weights (if necessary)
create_ens_data_reg <- reactive({
  if (create_ens_reg) {
    browser()

  } else {
    create_ens_data_rescale()
  }
})

### Rescale predictions
create_ens_data_rescale <- reactive({
  models.overlaid <- vals$overlaid.models[create_ens_overlaid_idx()]
  x.pred.idx <- switch(
    as.numeric(input$create_ens_rescale_type),
    "none", "abundance", "normalization", "standardization", "sumto1"
  )

  if (x.pred.idx == "abundance") {
    validate(
      need(input$create_ens_rescale_abund > 0,
           "Error: Abundance must be greater than 0 to rescale predictions")
    )
  }

  if (x.pred.idx == "none") {
    temp <- models.overlaid
  } else {
    temp <- eSDM::ensemble_rescale(
      models.overlaid, rep("Pred.overlaid", length(models.overlaid)),
      x.pred.idx, input$create_ens_rescale_abund
    )
  }

  # Next level-up function expects data.frame of prediction values
  data.frame(lapply(temp, function(i) st_set_geometry(i, NULL)$Pred.overlaid)) %>%
    purrr::set_names(letters[1:length(temp)])
})


###############################################################################
# 'Level 3' functions

#################################################
### Get indices of overlaid models to be used in ensemble
# This is it's own func since it is used in a flag in server_3_createEns.R
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
# Functions that return strings with ensemble model info for reactive vars

### Indices of overlaid models used in ensemble
create_ens_info_overlaid_idx <- reactive({
  overlaid.idx <- create_ens_overlaid_idx()

  if (length(overlaid.idx) == length(vals$overlaid.models)) {
    "All"
  } else {
    paste(overlaid.idx, collapse = ", ")
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
           "3" = "Spatial by pixel")
  }
})

### Rescaling method used
create_ens_info_rescaling <- reactive ({
  switch(input$create_ens_rescale_type,
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
    paste("Predictions rescaled using the", str.rescale, "method")
  } else {
    str.rescale
  }
})

###############################################################################
