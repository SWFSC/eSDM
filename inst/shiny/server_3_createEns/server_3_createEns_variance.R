### Code for calculating and plotting among-model variance


###############################################################################
# Functions for calculating among-model variance. Specific to eSDM
#   Ensemble preds are in last column of i
#   j is model weights; will be equal for unweighted ensemble
variance_func_esdm <- function(i, j) {
  i.o <- unname(head(i, -1))
  i.e <- unname(tail(i, 1))

  if (sum(!is.na(i.o)) > 1 && !is.na(i.e)) {
    sum(((i.o - i.e) ^ 2) * j, na.rm = TRUE)

  } else {
    NA
  }
}


###############################################################################
# _temp because this needs to get changed to:
#   observeEvent() -> reactiveVal -> plot pipeline
# ens_var_temp <- eventReactive(input$ens_var_execute, {
#   var.sf <- ens_var_sf()
#   # browser()
#
#   plot(
#     var.sf["sd_val"], axes = TRUE, border = NA,
#     nbreaks = 6, breaks = "equal", key.length = 1
#   )
# })


### Calculate among-model variance
ens_var_sf <- reactive({
  e.which <- input$ens_datatable_ensembles_rows_selected
  req(length(e.which) == 1)

  #----------------------------------------------------
  ### Create data frame with rescaled overlaid preds and ensemble preds
  o.preds.res <- vals$ensemble.overlaid.res[[e.which]]
  o.count <- ncol(o.preds.res)

  e.preds <- vals$ensemble.models[[e.which]]$Pred.ens

  pred.all <- data.frame(o.preds.res, e.preds) %>%
    purrr::set_names(c(paste0("o.rescale", 1:o.count), "ens_preds"))

  #----------------------------------------------------
  ### Get weights (1/n if unweighted) and calculate variance
  e.weights <- vals$ensemble.weights[[e.which]]
  if (is.na(e.weights)) { #i.e. unweighted
    pred.weights <- rep(1 / o.count, o.count)

  } else {
    pred.weights <- as.numeric(strsplit(e.weights, ", ")[[1]])
    pred.weights <- pred.weights / sum(pred.weights)
  }

  # validate(
  #   need(round(sum(pred.weights), 3) == 1,
  #        paste0("Error in among-model variance calculations;",
  #               "please report this as an issue"))
  # )
  stopifnot(round(sum(pred.weights), 3) == 1)

  st_sf(
    var_val = apply(pred.all, 1, variance_func_esdm, j = pred.weights),
    geometry = vals$overlay.base.sfc
  ) %>%
    mutate(sd_val = sqrt(var_val)) %>%
    st_set_agr("constant")
})
