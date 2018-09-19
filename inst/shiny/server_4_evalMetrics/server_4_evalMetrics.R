### Calculate AUC, TSS, or RMSE of selected models given p/a or numerical points
### 'p/a' or 'pa' refers to presence/absence points


###############################################################################
# Flags and resetting of inputs and reactiveValues

### Flag for if any model predictions are in app
output$eval_display_flag <- reactive({
  list.models.all <- list(
    vals$models.ll, vals$overlaid.models, vals$ensemble.models
  )

  any(sapply(list.models.all, length) > 0)
})
outputOptions(output, "eval_display_flag", suspendWhenHidden = FALSE)

### Flag for if validation data has been loaded
output$eval_display_calc_metrics_flag <- reactive({
  inherits(vals$eval.data, "sf")
})
outputOptions(output, "eval_display_calc_metrics_flag",
              suspendWhenHidden = FALSE)


###############################################################################
# Functions for calculating metrics

###########################################################
# Prep

### Generate indicies of models for which to calculate metrics
eval_models_idx <- reactive({
  eval.models.idx <- list(
    input$eval_models_table_orig_out_rows_selected,
    input$eval_models_table_over_out_rows_selected,
    input$eval_models_table_ens_out_rows_selected
  )

  lapply(eval.models.idx, function(i) {
    if (!is.null(i)) sort(i) else i
  })
})

### Generate list of sdms in native projections with which to calculate metrics
# Validating done in eval_metrics()
eval_models <- reactive({
  eval.models.idx <- eval_models_idx()

  models.list.orig <- vals$models.orig[eval.models.idx[[1]]]
  models.list.over <- vals$overlaid.models[eval.models.idx[[2]]]
  models.list.ens  <- vals$ensemble.models[eval.models.idx[[3]]]

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
  models.idx.any <- any(!sapply(eval_models_idx(), is.null))
  which.metrics <- input$eval_metrics_which

  # All validating done here so all messages are displayed at same time
  validate(
    need(inherits(eval.data, "sf"),
         paste("Error: Please load validation data in order",
               "to calculate model evaluation metrics")) %then%
      need(models.idx.any,
           paste("Error: Please select at least one model for which ",
                 "to calculate model evaluation metrics")) %then%
      need(!is.null(which.metrics),
           "Error: Please select at least one evaluation metric to calculate")
  )

  # Calculate metrics
  withProgress(message = 'Evaluating models', value = 0, {
    models.toeval <- eval_models()
    m.num <- length(models.toeval)
    incProgress(0.1)

    eval.results <- mapply(function(m, idx) {
      eval.data <- st_transform(eval.data, st_crs(m))
      # eSDM function removes NA predictions

      incProgress(
        amount = 0.8 / m.num,
        detail = paste("Calculating metrics for model", idx, "of", m.num)
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

    incProgress(0.1, "Processing metrics")
    eval.results <- eval.results[c("AUC", "TSS", "RMSE") %in% which.metrics, ]
  })

  # Save model idx and metrics to reactiveValues
  vals$eval.models.idx <- eval_models_idx()
  vals$eval.metrics <- eval.results
  vals$eval.metrics.names <- which.metrics

  ""
})


###############################################################################
# Evaluation metrics table

###########################################################
### Generate table of calculated metrics
table_eval_metrics <- reactive({
  req(vals$eval.metrics, vals$eval.metrics.names)

  metrics.table <- as.data.frame(t(as.data.frame(vals$eval.metrics)))
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


### Generate message detailing number of points that landed on poly boundaries
eval_metrics_overlap <- eventReactive(input$eval_metrics_execute, {
  req(vals$eval.data)
  models.toeval <- eval_models()

  pt.over.len <- sapply(
    lapply(models.toeval, function(m) {
      eval.data <- st_transform(vals$eval.data, st_crs(m))
      which(sapply(suppressMessages(st_intersects(eval.data, m)), length) > 1)
    }),
    length
  )

  # Make text pretty
  if (length(pt.over.len) == 1) {
    paste(
      "The predictions being evaluated had", pt.over.len, "validation points",
      "that fell on the boundary between two or more prediction polygons;" ,
      "the predictions from these polygons were averaged for the evaluation"
    )

  } else {
    if (zero_range(pt.over.len)) {
      temp <- paste(
        "The predictions being evaluated each had", unique(pt.over.len)
      )

    } else if (length(pt.over.len) == 2) {
      temp <- paste(
        "The predictions being evaluated had", paste(pt.over.len, collapse = " and ")
      )

    } else {
      temp <- paste(
        "The predictions being evaluated had",
        paste0(paste(head(pt.over.len, -1), collapse = ", "), ","),
        "and", tail(pt.over.len, 1)
      )
    }

    paste(
      temp, "validation points, respectively,",
      "that fell on the boundary between two or more prediction polygons;" ,
      "the predictions from these polygons were averaged for the evaluation"
    )
  }
})


###########################################################
### Download table
# Currently set for no Error column: #'[, -3]' is to remove Error column
output$eval_metrics_table_save <- downloadHandler(
  filename = "eSDM_evaluation_metrics.csv",

  content = function(file) {
    eval.metrics <- table_eval_metrics()
    models.which <- vals$eval.models.idx

    ### Get info of models that have eval metrics calculated for them
    orig.table <- cbind(table_orig(), table_orig_stats()[, -1])
    orig.table <- orig.table[models.which[[1]], ]
    over.table <- table_overlaid()[models.which[[2]], ]
    ens.table  <- table_ensembles()[models.which[[3]], ]

    if (!is.null(orig.table)) {if (nrow(orig.table) == 0) orig.table <- NULL}
    if (!is.null(over.table)) {if (nrow(over.table) == 0) over.table <- NULL}
    if (!is.null(ens.table))  {if (nrow(ens.table) == 0)  ens.table <- NULL}

    ### Create and set column names as necessary
    if ((!is.null(orig.table) | !is.null(over.table)) & !is.null(ens.table)) {
      if (!is.null(orig.table)) {
        all.models.names <- c(
          paste(names(orig.table), names(ens.table), sep = "/")[1:4],
          names(orig.table)[5:9]
        )
        names(orig.table) <- all.models.names

      } else {
        all.models.names <- c(
          paste(names(over.table), names(ens.table), sep = "/")[1:4],
          names(over.table)[5:9]
        )
      }

      if (!is.null(over.table)) names(over.table) <- all.models.names

      ens.table <- purrr::set_names(
        cbind(ens.table, NA, NA, NA, NA, NA), all.models.names
      )
    }
    # Else: Either some combo of orig.table and over.table or it's only
    #        the ens.table. Thus, names are already correct.


    ### Combine info tables
    models.list.all <- list(orig.table, over.table, ens.table)
    models.list.all <- models.list.all[!sapply(models.list.all, is.null)]

    need.check.table <- all(sapply(models.list.all, function(j, names.1) {
      names(j) == names(models.list.all[[1]])
    }, names.1 = models.list.all[[1]]))

    req(zero_range(sapply(models.list.all, ncol)), need.check.table)
    all.models.info <- do.call(rbind, models.list.all)


    ### Combine metric table and info table
    req(nrow(all.models.info) == nrow(eval.metrics))
    eval.metrics.models.info <- cbind(eval.metrics, all.models.info)


    ### Write csv
    write.csv(eval.metrics.models.info, file = file, row.names = FALSE)
  }
)

###############################################################################
