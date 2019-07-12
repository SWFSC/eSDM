### Code for creating weighted ensembles


###############################################################################
###############################################################################
# Weighted ensemble method 1: 'Manual entry'

### Process text inputs for weights and return vector of weights
create_ens_weights_manual <- reactive({
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

  if (input$create_ens_table_subset) {
    models.num <- length(input$create_ens_datatable_rows_selected)
  } else {
    models.num <- length(vals$overlaid.models)
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


###############################################################################
###############################################################################
# Weighted ensemble method 2: 'Evaluation metric'

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

### Return vector of weights based on evaluation metrics
create_ens_weights_metric <- reactive({
  # Check that selected predictions have calculated metrics
  validate(
    need(all(create_ens_overlaid_idx() %in% vals$eval.models.idx[[2]]),
         paste("Error: You must calculate at least one metric for all",
               "selected overlaid predictions"))
  )
  create_ens_weights_metric_table()[, 3]
})


###############################################################################
###############################################################################
# Weighted ensemble method 3: 'Pixel-level spatial weights'

### Vector of idx of selected overlaid models that have spatial weights
create_ens_weights_pix_which <- reactive({
  which(!is.na(vapply(vals$overlaid.specs, function(i) i["col_weight"], "1")))
})

### Generate data frame of pixel weights - fed into table and ensemble_create
create_ens_weights_pix <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.which.spatial <- create_ens_weights_pix_which()

  # Need validate() call here for ensemble function
  validate(
    need(any(ens.which.spatial %in% ens.which),
         paste("Error: At least one of the selected overlaid predictions",
               "must have pixel-level spatial weights"))
  )

  w.list <- lapply(ens.which, function(i, j, k) {
    if (i %in% j) vals$overlaid.models[[i]]$Weight else rep(1, k)
  }, j = ens.which.spatial, k = nrow(vals$overlaid.models[[1]]))

  purrr::set_names(data.frame(w.list), paste0("w", seq_along(ens.which)))
})

### Table summarizing pixel-level spatial weights of selected overlaid preds
create_ens_weights_pix_table <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.which.spatial <- create_ens_weights_pix_which()

  # Before create_ens_weights_pix() call to avoid 'Error' validation
  validate(
    need(any(ens.which.spatial %in% ens.which),
         paste("At least one of the selected overlaid predictions must have",
               "pixel-level spatial weights to use this weighting method")),
    errorClass = "validation2"
  )

  ens.pix.w <- create_ens_weights_pix()

  data.frame(
    Predictions = paste("Overlaid", ens.which),
    Min = vapply(ens.pix.w, min, 1, na.rm = TRUE),
    Median = vapply(ens.pix.w, median, 1, na.rm = TRUE),
    Mean = vapply(ens.pix.w, mean, 1, na.rm = TRUE),
    Max = vapply(ens.pix.w, max, 1, na.rm = TRUE),
    NAs = vapply(ens.pix.w, function(i) sum(is.na(i)), 1)
  )
})


###############################################################################
###############################################################################
# Weighted ensemble method 4: Weighting by the inverse of the variance

### Vector of idx of selected overlaid preds that have associated uncertainty
create_ens_weights_var_which <- reactive({
  which(!is.na(vapply(vals$overlaid.specs, function(i) i["col_se"], "1")))
})

### Table summarizing variance values of selected overlaid preds
create_ens_weights_var_table <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.which.var <- create_ens_weights_var_which()

  # Need validate() call here for display in-app
  validate(
    need(all(ens.which %in% ens.which.var),
         paste("All of the selected overlaid predictions must have",
               "associated uncertainty values to use this weighting method")),
    errorClass = "validation2"
  )

  ens.varvalue <- create_ens_data_rescale()[[2]]

  data.frame(
    Predictions = paste("Overlaid", ens.which),
    Min = vapply(ens.varvalue, min, 1, na.rm = TRUE),
    Median = vapply(ens.varvalue, median, 1, na.rm = TRUE),
    Mean = vapply(ens.varvalue, mean, 1, na.rm = TRUE),
    Max = vapply(ens.varvalue, max, 1, na.rm = TRUE),
    NAs = vapply(ens.varvalue, function(i) sum(is.na(i)), 1)
  )
})

### Create data frame of weights (1 / var)
# ensemble_create() will normalize each row so it sums to 1
create_ens_weights_var <- reactive({
  ens.which <- create_ens_overlaid_idx()
  ens.which.var <- create_ens_weights_var_which()

  # Need validate() call here for ensemble function
  validate(
    need(all(ens.which %in% ens.which.var),
         paste("Error: All of the selected overlaid predictions must have",
               "associated uncertainty values to use this weighting method"))
  )

  purrr::set_names(
    1 / create_ens_data_rescale()[[2]],
    paste0("w", seq_along(ens.which))
  )
})

###############################################################################
###############################################################################
