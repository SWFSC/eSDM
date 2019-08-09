### Reactive functions that return data frames for tables for various tabs
# Most tables use 'if... return()' rather than 'req()' so that
# subsequent reactive functions won't be stopped


###############################################################################
# Original predictions

#------------------------------------------------------------------------------
### Table of original predictions
table_orig <- reactive({
  if (length(vals$models.ll) == 0) return()

  data.frame(vals$models.names, t(as.data.frame(vals$models.data.names)),
             vapply(as.numeric(vals$models.pred.type), function(i) {
               switch(i, "Absolute density", "Relative density", "Abundance")
             }, character(1)),
             stringsAsFactors = FALSE) %>%
    `rownames<-`(paste("Original", seq_along(vals$models.names))) %>%
    purrr::set_names(
      c("SDM filename", "Prediction", "Uncertainty", "Weight",
        "Pred value type"))
})


#------------------------------------------------------------------------------
### Table of original predictions with stats
table_orig_stats <- reactive({
  req(table_orig())

  data.frame(vals$models.names, t(as.data.frame(vals$models.specs)),
             stringsAsFactors = FALSE) %>%
    `rownames<-`(paste("Original", seq_along(vals$models.names))) %>%
    purrr::set_names(
      c("SDM filename", "Resolution", "Polygon count",
        "Non-NA prediction count", "Abundance", "Long, lat range"))
})


###############################################################################
### Table of overlaid predictions in 'Create ensemble predictions' tab
table_overlaid <- reactive({
  if (length(vals$overlaid.models) == 0) return()

  data.frame(t(data.frame(vals$overlaid.specs)), stringsAsFactors = FALSE) %>%
    `rownames<-`(paste("Overlaid", seq_along(vals$overlaid.specs))) %>%
    purrr::set_names(
      c("SDM filename", "Prediction", "Uncertainty", "Weight",
        "Pred value type", "Resolution", "Polygon count",
        "Non-NA prediction count", "Abundance", "Long, lat range"))
})


###############################################################################
# Ensemble predictions

#------------------------------------------------------------------------------
### Table of created ensemble predictions
table_ensembles <- reactive({
  if (length(vals$ensemble.models) == 0) return()

  data.frame(t(data.frame(vals$ensemble.specs)), stringsAsFactors = FALSE) %>%
    `rownames<-`(paste("Ensemble", seq_along(vals$ensemble.specs))) %>%
    dplyr::select(1, 2, 4, 6) %>%
    purrr::set_names(
      c("Predictions used", "Rescaling method", "Ensemble method",
        "Uncertainty method"))
})

#------------------------------------------------------------------------------
### Table of created ensemble predictions with stats
table_ensembles_stats <- reactive({
  if (length(vals$ensemble.models) == 0) return()

  data.frame(t(data.frame(vals$ensemble.specs)), stringsAsFactors = FALSE) %>%
    `rownames<-`(paste("Ensemble", seq_along(vals$ensemble.specs))) %>%
    dplyr::select(1, 3, 4, 5) %>%
    purrr::set_names(
      c("Predictions used", "Regional exclusion for", "Ensemble method",
        "Weights"))
})

###############################################################################
# Other are rendered in their pertinent files

###############################################################################
