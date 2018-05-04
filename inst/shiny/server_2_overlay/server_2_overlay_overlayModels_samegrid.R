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
  
  # browser()
  #########################################################
  ### Same-grid overlay prep
  
  withProgress(message = "Preparing for same-grid overlay", value = 0.5, {
    overlay_samegrid_validate() #Functionally the req() statement for this func
    
    
    #####################################
    ### Reset/hide reactive values, preview plots, and eval metrics
    overlay_reset()
    incProgress(0.3)
    
    #####################################
    ### Model overlay prep. ***Same code as in overlay_all() for this section
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
    incProgress(0.2)
  })
  
  #########################################################
  ### Perform same-grid 'overlay'
  
  withProgress(message = "Performing same-grid overlay", value = 0.3, {
    #####################################
    # Create base grid and spdf of model predictions used as grid
    # ***Same code as in overlay_all() for this section
    
    base.spdf <- overlay_create_base_spdf()
    names(base.spdf) <- c("Pred.overlaid", "Error.overlaid", 
                          "Weight.overlaid", "Pixels")
    base.sp <- as(base.spdf, "SpatialPolygons")
    
    # Get specs of base.sp
    base.specs <- vals$models.specs[[base.idx]]
    base.specs[2] <- length(base.sp)
    base.specs[3:4] <- NA
    if (identical(crs(base.sp), crs.ll)) {
      base.sp.ll <- base.sp
    } else {
      base.sp.ll <- spTransform(base.sp, crs.ll)
    }
    base.specs[5] <- paste(as.character(round(extent(base.sp.ll), 0)), 
                           collapse = ", ")
    
    # Store data
    #*** vals$base.sp set below
    vals$overlay.base.specs <- base.specs
    incProgress(0.1)
    
    #####################################
    # Create 'overlaid' models
    
    ### Make sure models are valid
    models.to.overlay <- lapply(vals$models.orig, function(spdf) {
      if (!suppressWarnings(gIsValid(spdf))) {
        spdf <- valid.poly(spdf, "Overlaid model predictions")
      }
      spdf
    })
    
    ### If study area or land polys are loaded, intersect models with base
    if (!is.null(vals$overlay.bound) | !is.null(vals$overlay.land)) {
      base.sp.union <- gUnaryUnion(base.sp)
      models.overlaid <- lapply(models.to.overlay, intersect, base.sp.union)
      
    } else {
      models.overlaid <- models.to.overlay
    }
    incProgress(0.2)
    
    ### Update names
    models.overlaid <- lapply(models.overlaid, function(spdf.overlaid) {
      names(spdf.overlaid) <- c("Pred.overlaid", "Error.overlaid", 
                                "Weight.overlaid", "Pixels")
      spdf.overlaid
    })
    
    # This ensures that polygon order is same for vals$ base and overlaid models
    vals$overlay.base.sp <- as(models.overlaid[[base.idx]], "SpatialPolygons")
    
    
    ### Save specs
    specs.list <- mapply(function(n, p) {
      if (p == 1) {
        n.abund <- unname(round(model.abundance(n, "Pred.overlaid")))
      } else {
        n.abund <- "N/A"
      }
      list(c(base.specs[1], length(n), sum(!is.na(n$Pred.overlaid)),
             n.abund, base.specs[5]))
    }, models.overlaid, vals$models.pred.type)

    vals$overlaid.models <- models.overlaid
    vals$overlaid.models.specs <- specs.list
    
    ###################################
    ### Ens.over prep
    incProgress(0.2, detail = "Finishing overlay process")
    overlay_ensemble_prep()
    incProgress(0.3)
  })
  
  #########################################################
  if (all(c(gIsValid(base.spdf), gIsValid(base.sp), 
            sapply(models.overlaid, gIsValid)))) {
    "Same-grid overlay performed successfully"
  } else {
    "Same-grid overlay performed, but outputs invalid. Please restart eSDM."
  }
})
  
  ###############################################################################
  