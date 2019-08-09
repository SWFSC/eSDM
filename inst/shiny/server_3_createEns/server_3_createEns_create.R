### Code for creating ensemble predictions from overlaid predictions


###############################################################################
### Top-level function for creating ensemble
create_ensemble <- eventReactive(input$create_ens_create_action, {
  # For the validation message
  table_overlaid()
  valid.txt <- "Error in creating ensemble - please report as an issue"

  withProgress(message = "Creating ensemble predictions", value = 0.6, {
    #------------------------------------------------------
    ### Predictions to be included in the ensemble
    ens.preds <- create_ens_data_reg()[[1]]
    ens.var   <- create_ens_data_reg()[[2]]

    ### Ensemble weights
    if (input$create_ens_type == "1") { #Unweighted
      ens.w <- create_ens_weights_unweighted()

    } else if (input$create_ens_type == "2") { #Weighted
      ens.w <- switch(
        input$create_ens_weight_type,
        "1" = create_ens_weights_manual(),
        "2" = create_ens_weights_metric(),
        "3" = create_ens_weights_pix(),
        "4" = create_ens_weights_var()
      )
    } else {
      validate(valid.txt)
    }

    #------------------------------------------------------
    ### Data format checks
    if (inherits(ens.w, "numeric")){
      validate(
        need(ncol(ens.preds) == length(ens.w), valid.txt)
      )
    } else if (inherits(ens.w, "data.frame")){
      validate(
        need(ncol(ens.preds) == ncol(ens.w) & nrow(ens.preds) == nrow(ens.w),
             valid.txt)
      )
    } else {
      validate(valid.txt)
    }

    #------------------------------------------------------
    ### Create ensemble
    if (input$create_ens_create_uncertainty == 2) { #WMV
      ens.df <- eSDM::ensemble_create(
        x = cbind(ens.preds, ens.var), x.idx = names(ens.preds),
        x.var.idx = names(ens.var),
        w = ens.w, na.rm = TRUE
      )

    } else { #AMV
      ens.df <- eSDM::ensemble_create(
        x = ens.preds, x.idx = names(ens.preds),
        x.var.idx = NULL,
        w = ens.w, na.rm = TRUE
      )
    }

    ens.df <- ens.df %>%
      dplyr::mutate(SE_ens = sqrt(Var_ens)) %>%
      dplyr::select(Pred_ens, SE_ens)

    incProgress(0.3)

    #------------------------------------------------------
    ### Add data to reactive variables
    vals$ensemble.models <- c(vals$ensemble.models, list(ens.df))
    vals$ensemble.overlaid.res <- c(
      vals$ensemble.overlaid.res, list(ens.preds)
    )

    vals$ensemble.specs <- c(
      vals$ensemble.specs,
      list(c(
        idx = create_ens_info_overlaid_idx(),
        res = create_ens_info_rescaling(),
        regexc = create_ens_info_regexc(),
        ensmethod = create_ens_info_weighting(),
        weights = create_ens_info_weights(),
        var = create_ens_info_uncertainty()
      ))
    )

    incProgress(0.1)
  })

  "Created ensemble; see table below for ensemble details"

  # paste0(
  #   "Created ensemble with:",
  #   tags$br(),
  #   create_ens_info_rescaling_message(), ";",
  #   tags$br(),
  #   ifelse(input$create_ens_reg,
  #          paste("Regional exclusion for", create_ens_info_regexc()),
  #          "No regional exclusion"), ";",
  #   tags$br(),
  #   paste(paste0("'", create_ens_info_weighting(), "'"), "ensemble method;"),
  #   tags$br(),
  #   paste(create_ens_info_uncertainty(), "uncertainty")
  # )
})


###############################################################################
### 'Level 1' functions - create vector or data frame of ensemble weights

# 'Weights' for unweighted ensemble
create_ens_weights_unweighted <- reactive({
  overlaid.count <- ncol(create_ens_data_reg()[[1]])
  rep(1 / overlaid.count, overlaid.count)
})

####################################################################
# Weights for weighted ensembles: 'ensCreateEns_create_weighted.R'
####################################################################


###############################################################################
# 'Level 2' functions

#------------------------------------------------------------------------------
### Return 2-element list of overlaid predictions df and SE values df
### After rescaling and applying regional exclusion as necessary
### create_ens_reg_exc() in 'ensCreateEns_create_regexc.R'
create_ens_data_reg <- reactive({
  if (input$create_ens_reg) {
    validate(
      need(sum(!sapply(vals$ens.over.wpoly.filename, is.null)) > 0,
           paste("Error: Please either upload and assign at least one",
                 "exclusion polygon or uncheck the",
                 "'Exclude specific regions...' checkbox"))
    )

    list(
      create_ens_data_rescale()[[1]] * create_ens_reg_exc(),
      create_ens_data_rescale()[[2]] * create_ens_reg_exc()
    )

  } else {
    create_ens_data_rescale()
  }
})

#------------------------------------------------------------------------------
### Return list(rescaled overlaid predictions df, rescaled SE values df)
create_ens_data_rescale <- reactive({
  models.overlaid <- vals$overlaid.models[create_ens_overlaid_idx()]
  j <- seq_along(models.overlaid)

  overlaid.sf <- data.frame(lapply(models.overlaid, dplyr::select, Pred)) %>%
    bind_cols(data.frame(lapply(models.overlaid, dplyr::select, SE)) ^ 2) %>%
    purrr::set_names(paste0("Pred", j), paste0("Var", j)) %>%
    st_sf(geometry = vals$overlay.base.sfc, agr = "constant")

  rescale.type <- switch(
    as.numeric(input$create_ens_rescale_type),
    "none", "abundance", "sumto1"
  )

  if (rescale.type == "abundance") {
    validate(
      need(input$create_ens_rescale_abund > 0,
           paste("Error: Abundance must be greater than 0 to rescale",
                 "predictions using the abundance method"))
    )
  }

  if (rescale.type == "none") {
    temp <- overlaid.sf

  } else {
    p.idx <- names(st_set_geometry(overlaid.sf, NULL))[j]
    v.idx <- names(st_set_geometry(overlaid.sf, NULL))[j + max(j)]

    temp <- try(eSDM::ensemble_rescale(
      x = overlaid.sf, x.idx = p.idx, x.var.idx = v.idx,
      y = rescale.type, y.abund = input$create_ens_rescale_abund
    ), silent = TRUE)

    validate(
      need(temp, "Error: error in rescaling, please report this as an issue")
    )
  }

  # For GUI, next function expects data frame of prediction values
  temp <- st_set_geometry(temp, NULL)

  list(
    temp %>% dplyr::select(starts_with("Pred")),
    temp %>% dplyr::select(starts_with("Var"))
  )
})


###############################################################################
# 'Level 3' functions

#------------------------------------------------------------------------------
### Get indices of overlaid prediction to be included in ensemble
# This is its own func since it is used in a flag in server_3_createEns.R
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

#------------------------------------------------------------------------------
### Get indices of overlaid models to be used in ensemble, and validate >= 2
create_ens_overlaid_idx <- reactive({
  req(length(vals$overlaid.models) > 0)

  ens.models.which <- create_ens_overlaid_idx_num()

  validate(
    need(length(ens.models.which) >= 2,
         paste("Error: Please select at least two sets of",
               "overlaid predictions for the ensemble"))
  )

  ens.models.which
})


###############################################################################
# Functions that return strings with ensemble info for vals

#------------------------------------------------------------------------------
### 1) Indices of overlaid predictions used in ensemble
create_ens_info_overlaid_idx <- reactive({
  overlaid.idx <- create_ens_overlaid_idx()

  if (length(overlaid.idx) == length(vals$overlaid.models)) {
    "All overlaid"
  } else {
    paste("Overlaid", paste(overlaid.idx, collapse = ", "))
  }
})

#------------------------------------------------------------------------------
### 2) Rescaling method used
create_ens_info_rescaling <- reactive ({
  switch(
    as.numeric(input$create_ens_rescale_type),
    "None", paste("Abundance:", input$create_ens_rescale_abund), "Sum to 1"
  )
})

#------------------------------------------------------------------------------
### 3) Regional exclusion - on which overlaid predictions
create_ens_info_regexc <- reactive({
  if (input$create_ens_reg) {
    overlaid.idx <- create_ens_overlaid_idx()
    regexc.idx <- which(
      !vapply(vals$ens.over.wpoly.sf, is.null, as.logical(1))
    )

    paste(
      "Overlaid",
      paste(regexc.idx[regexc.idx %in% overlaid.idx], collapse = ", ")
    )

  } else {
    "N/A"
  }
})

#------------------------------------------------------------------------------
### 4) Ensemble method used
create_ens_info_weighting <- reactive({
  if (input$create_ens_type == 1) {
    "Unweighted"

  } else {
    tmp <- as.numeric(input$create_ens_weight_type)
    if (tmp == 2) {
      paste("Weighted -", paste0(input$create_ens_weights_metric, "-based"))
    } else {
      txt.vec <- c("Manual", NA, "Pixel-level spatial weights", "Uncertainty")
      paste("Weighted -", txt.vec[tmp])
    }
  }
})

#------------------------------------------------------------------------------
### 5) Weights used, as applicable
# Get weights from functions that returns numerical weights for computation
create_ens_info_weights <- reactive({
  if (input$create_ens_type == 1) {
    "N/A"

  } else {
    switch(
      as.numeric(input$create_ens_weight_type),
      input$create_ens_weight_manual,
      paste(
        format(round(create_ens_weights_metric_table()$Weights, 3),
               nsmall = 3, justify = "right"),
        collapse = ", "
      ),
      "Spatial by pixel", "Inverse of variance"
    )
  }
})

#------------------------------------------------------------------------------
### 6) Uncertainty method - AMV or WMV
create_ens_info_uncertainty <- reactive({
  ifelse(
    input$create_ens_create_uncertainty == 1, "Among-model", "Within-model"
  )
})


###############################################################################
### Generate string for text about created ensemble
create_ens_info_rescaling_message <- reactive({
  input.rescale <- input$create_ens_rescale_type
  str.rescale <- switch(
    as.numeric(input.rescale),
    "Predictions not rescaled", "abundance", "sum to 1"
    # "normalization", "standardization", "sum to 1"
  )

  if (input.rescale != 1) {
    paste("Predictions rescaled using the", str.rescale, "method")
  } else {
    str.rescale
  }
})

###############################################################################
