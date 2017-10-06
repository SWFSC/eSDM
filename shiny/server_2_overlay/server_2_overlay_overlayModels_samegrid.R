### Overlay models when all models were made on the same base grid


###############################################################################
# Perform checks to ensure that preds are actually on the same grid

#################################################
### Warning messages for checks that should be TRUE to use functionality
overlay_samegrid_warning <- reactive({
  req(length(vals$models.orig) > 0)
  
  # Quick error check that each has same number of predictions
  validate(
    need(zero_range(sapply(vals$models.orig, length)),
         paste("Error: Loaded model predictions have different cell counts.", 
               "You cannot perform a same-grid overlay with predictions", 
               "that have different cell counts."))
  )
  
  # Perform warning checks
  warning.check.1 <- all(sapply(vals$models.names, identical, 
                                vals$models.names[[1]]))
  warning.check.2 <- all(sapply(vals$models.pred.type, identical, 
                                vals$models.pred.type[[1]]))
  warning.check.3.list <- lapply(vals$models.specs, function(i) i[c(1, 5)])
  warning.check.3 <- all(sapply(warning.check.3.list, identical, 
                                warning.check.3.list[[1]]))
  warning.check.all <- c(warning.check.1, warning.check.2, warning.check.3)
  
  # Warning messages for checks
  warning.messages <- c(
    "Warning: The predictions come from different files", 
    "Warning: The predicitons have different prediction types", 
    paste("Warning: The predictions have different resolutions and/or", 
          "longitude and latitude ranges")
  )
  
  
  # Return warning messages for checks that were FALSE
  if (any(!warning.check.all)) {
    paste(warning.messages[!warning.check.all], collapse = "<br/>")
  } else {
    NULL
  }
})


#################################################
### Validate messages for checks that must be TRUE to use functionality
# Some checks are redundant, but done in this order for sake of time
overlay_samegrid_validate <- reactive({
  req(length(vals$models.orig) > 0)
  
  # Check that each has same number of predictions
  validate(
    need(zero_range(sapply(vals$models.orig, length)),
         "Error: Loaded model predictions have different cell counts")
  )
  
  # Check that crs codes are the same
  sp.crs <- lapply(vals$models.orig, crs)
  validate(
    need(all(sapply(sp.crs, identical, sp.crs[[1]])),
         paste("Error: The grid structure is not the same", 
               "for all loaded model predictions"))
  )
  
  # Check that SpatialPolygons structure is the same for all models
  # This checks for equal: area, order of polygons
  sp.orig <- lapply(vals$models.orig, as, "SpatialPolygons")
  validate(
    need(all(sapply(sp.orig, identical, sp.orig[[1]])),
         paste("Error: The grid structure is not the same", 
               "for all loaded model predictions"))
  )
  
  TRUE
})

###############################################################################
### Do overlay of predictions that are already on the same grid
overlay_samegrid_all <- eventReactive(input$overlay_samegrid_overlay_execute, {
  overlay_samegrid_validate() #Functionally the req() statement for this func

  browser()
  #########################################################
  ### Reset/hide reactive values, preview plots, and eval metrics
  overlay_reset()
  
  #########################################################
  ### Model overlay prep. Same code as in overlay_all() for this section
  # Get index of model predictions to be base grid
  base.idx <- as.numeric(input$overlay_loaded_table_rows_selected)
  models.num <- length(vals$models.ll)
  
  validate(
    need(length(base.idx) == 1, 
         paste("Error: Please select exactly one model from the table", 
               "to use as the base grid")), 
    need(models.num > 1, 
         paste("Error: Please add more than one model to the app", 
               "before overlaying")), 
    if (input$overlay_bound) {
      need(!is.null(vals$overlay.bound), 
           paste("Error: Please either uncheck boundary box", 
                 "or load a boundary polygon"))
    }, 
    if (input$overlay_land) {
      need(!is.null(vals$overlay.land),
           "Error: Please either uncheck land box or load a land polygon")
    }
  )
  
  vals$overlay.base.idx <- base.idx
  
  # Get crs of projection to be used in overlay process
  vals$overlay.crs <- overlay_crs()
  
  #########################################################
  ### 
  
  
})

###############################################################################
