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

# Used by both ens_var_sf() and [pretty plot]
# Calculate variance and return sf object with var and sd
# Can only be used within the eSDM; calls vals within function
ens_var_helper_esdm <- function(e.which) {
  #--------------------------------------------------------
  # Create data frame with rescaled overlaid preds and ensemble preds
  o.preds.res <- vals$ensemble.overlaid.res[[e.which]]

  e.preds <- vals$ensemble.models[[e.which]]$Pred.ens

  pred.all <- data.frame(o.preds.res, e.preds) %>%
    purrr::set_names(c(paste0("o.rescale", 1:ncol(o.preds.res)), "ens_preds"))

  # Get ens weights (if any)
  e.weights <- vals$ensemble.weights[[e.which]]

  #--------------------------------------------------------
  o.count <- ncol(pred.all) - 1

  if (is.na(e.weights)) { #i.e. unweighted
    pred.weights <- rep(1 / o.count, o.count)

  } else {
    pred.weights <- as.numeric(strsplit(e.weights, ", ")[[1]])
    pred.weights <- pred.weights / sum(pred.weights)
  }

  if (round(sum(pred.weights), 3) != 1) {
    stop("Error in the processing of the ensemble weights; ",
         "please report this as an issue")
  }

  #--------------------------------------------------------
  st_sf(var_val = apply(pred.all, 1, variance_func_esdm, j = pred.weights),
        geometry = vals$overlay.base.sfc) %>%
    mutate(se_val = sqrt(var_val)) %>%
    st_set_agr("constant")
}


###############################################################################
### Calculate among-model variance
ens_var_sf <- reactive({
  e.which <- input$ens_datatable_ensembles_rows_selected
  req(length(e.which) == 1)

  ens_var_helper_esdm(e.which)
})
