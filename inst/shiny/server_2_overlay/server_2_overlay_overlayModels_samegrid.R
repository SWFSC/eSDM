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
  models.orig.crs <- lapply(vals$models.orig, st_crs)
  validate(
    need(all(sapply(models.orig.crs, identical, models.orig.crs[[1]])),
         paste("Error: The coordinate system is not the same",
               "for all loaded model predictions"))
  )

  # Check that sfc structure is the same for all models
  # This checks for equal: area, order of polygons
  models.orig.sfc <- lapply(vals$models.orig, st_geometry)
  validate(
    need(all(sapply(models.orig.sfc, identical, models.orig.sfc[[1]])),
         paste("Error: The grid structure is not the same",
               "for all loaded model predictions"))
  )

  TRUE
})

###############################################################################
### Do overlay of predictions that are already on the same grid
overlay_samegrid_all <- eventReactive(input$overlay_samegrid_overlay_execute, {
  #########################################################
  ### Same-grid overlay prep
  withProgress(message = "Preparing for same-grid overlay", value = 0.5, {
    overlay_samegrid_validate() #Basically the req() statement for this func

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

    base.sf <- overlay_create_base_sf()
    names(base.sf)[1:4] <- c("Pred.overlaid", "Error.overlaid",
                             "Weight.overlaid", "Pixels")
    base.sfc <- st_geometry(base.sf)

    # Get specs of base.sfc
    base.specs <- vals$models.specs[[base.idx]]
    base.specs[2] <- length(base.sfc)
    base.specs[3:4] <- NA
    if (identical(st_crs(base.sfc), crs.ll)) {
      base.sfc.ll <- base.sfc
    } else {
      base.sfc.ll <- st_transform(base.sfc, crs.ll)
    }
    base.specs[5] <- paste(as.character(round(st_bbox(base.sfc.ll)[c(1, 3, 2, 4)], 0)),
                           collapse = ", ")

    # Store data; vals$base.sfc set below
    vals$overlay.base.specs <- base.specs
    incProgress(0.1)

    #####################################
    # Create 'overlaid' models
    browser()
    validate(need(FALSE, "This is not functional"))
    models.preoverlay <- vals$models.ll[-base.idx]


    ### If study area or land polys are loaded, intersect models with base
    if (!is.null(vals$overlay.bound) | !is.null(vals$overlay.land)) {
      base.sfc.union <- aggregate(base.sfc, do_union = TRUE)
      models.preoverlay <- lapply(models.preoverlay, st_intersection, base.sfc.union)

    }
    incProgress(0.2)

    ### Update names
    models.overlaid <- lapply(models.preoverlay, function(i) {
      names(i)[1:4] <- c("Pred.overlaid", "Error.overlaid",
                         "Weight.overlaid", "Pixels")
      i
    })

    # This ensures that polygon order is same for vals$ base and overlaid models
    vals$overlay.base.sfc <- as(models.overlaid[[base.idx]], "SpatialPolygons")


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
  if (all(c(gIsValid(base.sf), gIsValid(base.sp),
            sapply(models.overlaid, gIsValid)))) {
    "Same-grid overlay performed successfully"
  } else {
    "Same-grid overlay performed, but outputs invalid. Please restart eSDM."
  }
})

###############################################################################
