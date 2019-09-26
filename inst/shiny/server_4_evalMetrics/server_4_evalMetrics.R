# Calculate AUC, TSS, or RMSE of selected predictions given
#   count or pres/abs points
# 'p/a' or 'pa' refers to presence/absence points


###############################################################################
# Flags and resetting of inputs and reactiveValues

### Flag for if any predictions are imported or created
output$eval_display_flag <- reactive({
  list.models.all <- list(
    vals$models.ll, vals$overlaid.models, vals$ensemble.models
  )
  any(vapply(list.models.all, length, 1) > 0)
})
outputOptions(output, "eval_display_flag", suspendWhenHidden = FALSE)

### Flag for if validation data has been imported
output$eval_display_calc_metrics_flag <- reactive({
  inherits(vals$eval.data, "sf")
})
outputOptions(
  output, "eval_display_calc_metrics_flag", suspendWhenHidden = FALSE
)


###############################################################################
# Functions for calculating metrics

###########################################################
# Prep

### Get indicies of predictions for which to calculate metrics
eval_models_idx <- reactive({
  eval.models.idx <- list(
    input$eval_models_table_orig_out_rows_selected,
    input$eval_models_table_over_out_rows_selected,
    input$eval_models_table_ens_out_rows_selected
  )

  lapply(eval.models.idx, function(i) {if (isTruthy(i)) sort(i) else i})
})

### Generate list of sdms in native projections with which to calculate metrics
# Validating done in eval_metrics()
eval_models <- reactive({
  eval.models.idx <- eval_models_idx()

  models.list.orig <- vals$models.orig[eval.models.idx[[1]]]
  models.list.over <- lapply(
    vals$overlaid.models[eval.models.idx[[2]]],
    st_sf, geometry = vals$overlay.base.sfc, agr = "constant"
  )
  models.list.ens  <- lapply(
    vals$ensemble.models[eval.models.idx[[3]]],
    st_sf, geometry = vals$overlay.base.sfc, agr = "constant"
  )

  c(models.list.orig, models.list.over, models.list.ens)
})


###########################################################
### Calculate metrics
eval_metrics <- eventReactive(input$eval_metrics_execute, {
  # Reset eval reactiveValues
  vals$eval.metrics <- NULL
  vals$eval.metrics.names <- NULL
  vals$eval.models.idx <- NULL

  # Set necessary variables
  eval.data <- vals$eval.data
  models.idx.any <- any(sapply(eval_models_idx(), isTruthy))
  which.metrics <- input$eval_metrics_which

  # All validate()'s here so all messages are displayed at same time
  validate(
    need(inherits(eval.data, "sf"),
         paste("Error: Please import validation data to",
               "calculate evaluation metrics")),
    need(models.idx.any,
         paste("Error: Please select at least one set of predictions for which ",
               "to calculate evaluation metrics")),
    need(!is.null(which.metrics),
         "Error: Please select at least one evaluation metric to calculate")
  )

  # Calculate metrics
  withProgress(message = 'Evaluating predictions', value = 0, {
    models.toeval <- eval_models()
    m.num <- length(models.toeval)
    incProgress(0.1)

    eval.results <- mapply(function(m, idx) {
      eval.data <- st_transform(eval.data, st_crs(m))
      # eSDM::evaluation_metrics() removes NA predictions

      incProgress(
        amount = 0.7 / m.num,
        detail = paste("Calculating metrics for predictions", idx, "of", m.num)
      )

      if (vals$eval.data.specs[2] == 1) {
        suppressMessages(
          eSDM::evaluation_metrics(m, names(m)[1], eval.data, "count", TRUE)
        )
      } else {
        suppressMessages(
          eSDM::evaluation_metrics(m, names(m)[1], eval.data, "sight")
        )
      }
    }, models.toeval, seq_along(models.toeval), SIMPLIFY = TRUE)

    incProgress(0.1, "Processing metrics", detail = "")
    eval.results <- eval.results[c("AUC", "TSS", "RMSE") %in% which.metrics, ]
    if (length(which.metrics) == 1) {
      eval.results <- matrix(eval.results, nrow = 1)
    }
    eval.overlap.message <- eval_overlap_message(eval_models(), eval.data)
    incProgress(0.1)

    # Save model idx and metrics to reactiveValues
    vals$eval.models.idx <- eval_models_idx()
    vals$eval.metrics <- list(eval.results, eval.overlap.message)
    vals$eval.metrics.names <- which.metrics
  })

  ""
})


###############################################################################
# Evaluation metrics table

###########################################################
### Generate table of calculated metrics
table_eval_metrics <- reactive({
  req(vals$eval.metrics, vals$eval.metrics.names)

  metrics.table <- as.data.frame(t(as.data.frame(vals$eval.metrics[[1]])))
  names(metrics.table) <- vals$eval.metrics.names

  models.idx <- vals$eval.models.idx
  isolate({
    table.orig <- table_orig()[models.idx[[1]], ]
    table.over <- table_overlaid()[models.idx[[2]], ]
    table.ensembles <- table_ensembles()[models.idx[[3]], ]
  })

  data.frame(
    "Predictions" = c(
      row.names(table.orig), row.names(table.over), row.names(table.ensembles)
    ),
    metrics.table, stringsAsFactors = FALSE
  )
})


###########################################################
### Download table
# Currently set for no Error column: #'[, -3]' is to remove Error column
output$eval_metrics_table_save <- downloadHandler(
  filename = "eSDM_evaluation_metrics.csv",

  content = function(file) {
    eval.metrics <- table_eval_metrics()
    models.which <- vals$eval.models.idx

    ### Get info of predictions that have eval metrics calculated for them
    # No full tidyverse for if tables are NULL (and to preserve row names)
    orig.table <- cbind(
      table_orig(), dplyr::select(table_orig_stats(), -"SDM filename")
    )
    orig.table <- orig.table[models.which[[1]], ]

    over.table <- table_overlaid()[models.which[[2]], ]

    ens.table <- cbind(
      table_ensembles(),
      dplyr::select(table_ensembles_stats(), -"Predictions used",
                    -"Ensemble method")
    )
    ens.table <- ens.table[models.which[[3]], ]

    if (!is.null(orig.table)) {if (nrow(orig.table) == 0) orig.table <- NULL}
    if (!is.null(over.table)) {if (nrow(over.table) == 0) over.table <- NULL}
    if (!is.null(ens.table))  {if (nrow(ens.table) == 0)  ens.table <- NULL}

    ### Create and set column names as necessary
    if ((!is.null(orig.table) | !is.null(over.table)) & !is.null(ens.table)) {
      curr.table <- if(!is.null(orig.table)) orig.table else over.table
      ens.ncol <- ncol(ens.table)

      all.models.names <- c(
        paste(names(curr.table), names(ens.table), sep = "/")[seq_len(ens.ncol)],
        names(curr.table)[(ens.ncol+1):ncol(curr.table)]
      )

      if (!is.null(orig.table)) names(orig.table) <- all.models.names
      if (!is.null(over.table)) names(over.table) <- all.models.names

      for (i in seq_len(length(all.models.names) - ens.ncol)) {
        ens.table <- cbind(ens.table, NA)
      }
      names(ens.table) <- all.models.names
    }
    # Else: Either some combo of orig.table and over.table,
    #   or it's only ens.table. Either way, names are already correct.

    ### Combine info tables
    models.list.all <- list(orig.table, over.table, ens.table)
    models.list.all <- models.list.all[!sapply(models.list.all, is.null)]

    need.check.table <- all(sapply(models.list.all, function(j, names.1) {
      names(j) == names.1
    }, names.1 = names(models.list.all[[1]])))

    req(zero_range(sapply(models.list.all, ncol)), need.check.table)
    all.models.info <- do.call(rbind, models.list.all)


    ### Combine metric table and info table and write csv
    req(nrow(all.models.info) == nrow(eval.metrics))
    write.csv(
      cbind(eval.metrics, all.models.info), file = file, row.names = FALSE
    )
  }
)

###############################################################################
