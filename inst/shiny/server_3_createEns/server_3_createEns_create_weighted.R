### Code for creating weighted ensembles


###############################################################################
###############################################################################
# Weighted ensembling method 1: 'Manual entry'

### Process text inputs for weights
create_ens_weights_num <- reactive({
  preds.weights <- suppressWarnings(
    esdm_parse_num(req(input$create_ens_weight_manual))
    # as.numeric(unlist(strsplit(req(input$create_ens_weight_manual), ",")))
  )

  validate(
    need(!anyNA(preds.weights),
         paste("Error: One or more of the weights was not recognized as",
               "a number; please ensure that all of the weights are numbers",
               "separated by a comma and a space"))
  )

  models.num <- length(vals$overlaid.models)
  if (input$create_ens_table_subset) {
    models.num <- length(input$create_ens_datatable_rows_selected)
  }

  # Validate weights input
  validate(
    need(length(preds.weights) == models.num,
         paste("Error: The number of entered weights does not",
               "match the number of selected overlaid predictions")) %then%
      need(all(preds.weights > 0),
           "Error: All entered weights must be greater than zero") %then%
      need(round(sum(preds.weights), 3) == 1,
           "Error: The entered weights do not sum to 1")
  )

  preds.weights
})


### Create weighted ensemble from manually entered weights
create_ens_weighted_manual <- reactive({
  data.rescaled <- create_ens_data_reg()
  base.sfc <- vals$overlay.base.sfc
  data.weights <- create_ens_weights_num()

  # Check that length of weights == length of overlaid models to ensemble
  validate(
    need(length(data.weights) == ncol(data.rescaled),
         paste("Error: Weighted ens 1: number of weights != number of",
               "overlaid predictions; please report this as an issue"))
  )

  data.ens <- data.frame(Pred.ens = apply(data.rescaled, 1, function(p) {
    stats::weighted.mean(p, data.weights, na.rm = TRUE)
  }))
  data.ens$Pred.ens[is.nan(data.ens$Pred.ens)] <- NA

  st_sf(data.ens, geometry = base.sfc, agr = "constant")
})


###############################################################################
###############################################################################
# Weighted ensembling method 2: 'Evaluation metric'

### Table of selected metrics
create_ens_weights_metric_table <- reactive({
  req(
    input$create_ens_weights_metric,
    all(create_ens_overlaid_idx() %in% vals$eval.models.idx[[2]])
  )

  # Get desired metric for desired overlaid models from eval metrics table
  eval.metrics <- table_eval_metrics()

  idx.col <- which(names(eval.metrics) == input$create_ens_weights_metric)
  idx.row <- grep("Overlaid", eval.metrics$Predictions)
  idx.row <- idx.row[vals$eval.models.idx[[2]] %in% create_ens_overlaid_idx()]

  weights.table <- eval.metrics[idx.row, c(1, idx.col)]

  # Prep for display
  weights.table$R.weights <- weights.table[, 2] / sum(weights.table[, 2])
  names(weights.table)[3] <- "Weights"
  row.names(weights.table) <- 1:nrow(weights.table)

  weights.table
})

### Create weighted ensemble using some evaluation metric as weights
create_ens_weighted_metric <- reactive({
  data.rescaled <- create_ens_data_reg()
  base.sfc <- vals$overlay.base.sfc

  # Check that any predictions have been calculated
  validate(
    need(isTruthy(vals$eval.metrics),
         paste("Error: You must calculate at least one metric for all",
               "selected overlaid predictions"))
  )
  data.weights <- create_ens_weights_metric_table()[, 3]

  # Check that length of weights == length of overlaid models to ensemble
  validate(
    need(length(data.weights) == ncol(data.rescaled),
         paste("Error: Weighted ens by metrics: number of weights != number",
               "of overlaid predictions; please report this as an issue"))
  )

  data.ens <- data.frame(Pred.ens = apply(data.rescaled, 1, function(p) {
    stats::weighted.mean(p, data.weights, na.rm = TRUE)
  }))
  data.ens$Pred.ens[is.nan(data.ens$Pred.ens)] <- NA

  st_sf(data.ens, geometry = base.sfc, agr = "constant")
})


###############################################################################
###############################################################################
# Weighted ensembling method 3: 'Pixel-level spatial weights'

### Vector of idx of selected overlaid models that have spatial weights
create_ens_weights_pix_which <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.overlaid <- vals$overlaid.models[ens.which]
  ens.which.spatial <- sapply(ens.overlaid, function(i) {
    any(!is.na(i$Weight.overlaid))
  })

  ens.which[ens.which.spatial]
})

### Table of selected overlaid models and if they have any spatial weights
create_ens_weights_pix_table <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.which.spatial <- create_ens_weights_pix_which()

  validate(
    need(any(ens.which.spatial %in% ens.which),
         paste("At least one of the selected overlaid predictions must have",
               "pixel-level spatial weights to use this weighting method")),
    errorClass = "validation2"
  )

  ens.which.spatial.text <- ifelse(
    ens.which %in% ens.which.spatial, "Yes", "No"
  )

  ens.which.spatial.text2 <- sapply(ens.which, function(i) {
    if (i %in% ens.which.spatial) {
      j <- na_which(vals$overlaid.models[[i]]$Weight.overlaid)
      ifelse(
        anyNA(j), 0,
        sum(!(j %in% na_which(vals$overlaid.models[[i]]$Pred.overlaid)))
      )

    } else {
      "N/A"
    }
  })

  data.frame(paste("Overlaid", ens.which), ens.which.spatial.text,
             ens.which.spatial.text2, stringsAsFactors = FALSE) %>%
    purrr::set_names(c("Predictions", "Has spatial weights",
                       "Count of non-NA predictions with NA weight values"))
})

### Generate data frame of pixel weights
create_ens_weights_pix_weights <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.which.spatial <- create_ens_weights_pix_which()
  validate( #need validate() here too for ensembling function
    need(any(ens.which.spatial %in% ens.which),
         paste("Error: At least one of the selected overlaid predictions",
               "must have pixel-level spatial weights"))
  )

  w.list <- lapply(ens.which, function(idx) {
    overlaid.curr <- vals$overlaid.models[[idx]]

    if (idx %in% ens.which.spatial) {
      overlaid.curr$Weight.overlaid
    } else {
      rep(1, nrow(overlaid.curr))
    }
  })
  purrr::set_names(data.frame(w.list), letters[1:length(ens.which)])
})

### Create weighted ensemble using pixel-level spatial weights
create_ens_weighted_pix <- reactive({
  data.rescaled <- create_ens_data_reg()
  base.sfc <- vals$overlay.base.sfc
  data.weights <- create_ens_weights_pix_weights()

  validate(
    need(ncol(data.weights) == ncol(data.rescaled),
         paste("Error: Weighted ens 3: number of weights != number",
               "of overlaid predictions; please report this as an issue")),
    need(nrow(data.weights) == nrow(data.rescaled),
         paste("Error: Weighted ens 3: len of weights != number",
               "of overlaid predictions; please report this as an issue"))
  )

  data.reweighted <- data.rescaled * data.weights
  data.ens <- data.frame(
    Pred.ens = apply(data.reweighted, 1, mean, na.rm = TRUE)
  )
  data.ens$Pred.ens[is.nan(data.ens$Pred.ens)] <- NA

  st_sf(data.ens, geometry = base.sfc, agr = "constant")
})

###############################################################################
###############################################################################
