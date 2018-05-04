### Get crs in which to do overlay
### Reset/hide reactive values, preview plots, and eval metrics
### Overlay models onto base grid and create vals$ens.over... objects

### Overlay predictions that are already on the same grid...
### ...functionality is in 'server_2_overlay_overlayModels_samegrid.R'


###############################################################################
### Get crs object with projection to be used in overlay process
# Currently this only handles the loaded model preds as possible inputs
overlay_crs <- reactive({
  if (input$overlay_samegrid_indicator == 2) {
    model.idx <- as.numeric(input$overlay_loaded_table_rows_selected)
    crs(vals$models.orig[[model.idx]])
    
  } else {
    if (input$overlay_proj_ll) {
      crs.ll
    } else {
      crs(vals$models.orig[[as.numeric(input$overlay_proj_which)]])
    }
  }
})


###############################################################################
### Reset vals$overlaid..., vals.ens.over..., vals$ensemble...,  
# and vals$eval... (if any overlaid metrics are calc'd) 
# before creating new overlaid
### Called in overlay_all() and overlay_samegrid_all()
overlay_reset <- reactive({
  vals$overlay.crs <- NULL
  vals$overlay.base.idx <- NULL
  vals$overlay.base.sp <- NULL
  vals$overlay.base.specs <- NULL
  vals$overlaid.models <- list()
  vals$overlaid.models.specs <- list()
  
  vals$ens.over.pix <- NULL
  vals$ens.over.wpoly.filename <- NULL
  vals$ens.over.wpoly.spdf <- NULL
  vals$ens.over.wpoly.coverage <- NULL
  
  vals$ensemble.models <- list()
  vals$ensemble.method <- NULL
  vals$ensemble.weights <- NULL
  vals$ensemble.rescaling <- NULL
  vals$ensemble.overlaid.idx <- NULL
  vals$ensemble.plotted.idx <- NULL
  
  # Could make this so it only removes overlaid metrics
  # TODO: make smarter
  if (!is.null(vals$eval.models.idx)) {
    if (!is.null(vals$eval.models.idx[[2]])){
      vals$eval.models.idx <- NULL
      vals$eval.metrics <- list()
      vals$eval.metrics.names <- NULL
    }
  }
  
  # Reset pretty vals if an overlaid or ensemble model was plotted
  if (!is.null(vals$pretty.plotted.idx)) {
    if (any(idx %in% vals$pretty.plotted.idx[[2]]) | 
        any(idx %in% vals$pretty.plotted.idx[[3]])) {
      shinyjs::hide("pretty_plot_plot", time = 0)
      vals$pretty.params.list <- list()
      vals$pretty.plotted.idx <- NULL
    }
  }
  
  ### Hide elements: this is done in server_hide_show.R
  
  TRUE
})


###############################################################################
### 
overlay_ensemble_prep <- reactive ({
  ### Make relavent vals$ens.... lists the length of the num of models
  list.null <- lapply(seq_along(vals$models.ll), function(d) NULL)
  vals$ens.over.wpoly.spdf     <- list.null
  vals$ens.over.wpoly.filename <- list.null
  vals$ens.over.wpoly.coverage <- list.null
  
  ### Use rasterize() to create an SPixDF of pixel nums for ensemble previews
  ens.sp <- vals$overlay.base.sp # base.sp is used as ensemble base
  ens.data <- data.frame(pix = 1:length(ens.sp))
  ens.spdf <- SpatialPolygonsDataFrame(ens.sp, ens.data, match.ID = FALSE)
  ens.crs <- crs(ens.spdf)
  
  if (!identical(ens.crs, crs.ll)) ens.spdf <- spTransform(ens.spdf, crs.ll)
  
  # Determine max of x and y extent and set raster dimensions appropriately
  ens.ext <- extent(ens.spdf)
  ens.xrange <- ens.ext@xmax - ens.ext@xmin
  ens.yrange <- ens.ext@ymax - ens.ext@ymin
  
  if (ens.xrange >= ens.yrange) {
    r.ncol <- 80
    r.res <- ens.xrange / r.ncol
    r.nrow <- round(ens.yrange / r.res, 0)
  } else { 
    r.nrow <- 80
    r.res <- ens.yrange / r.nrow
    r.ncol <- round(ens.xrange / r.res, 0)
  }
  
  r <- raster(ens.spdf, nrow = r.nrow, ncol = r.ncol)
  # rasterize takes the value of polygon that overlaps center of raster cell
  ens.raster <- rasterize(ens.spdf, r, field = "pix")
  ens.pix <- as(ens.raster, "SpatialPixelsDataFrame")
  names(ens.pix) <- "pix"
  
  vals$ens.over.pix <- ens.pix
  
  TRUE
})


###############################################################################
### Where the overlay magic aka science happens

overlay_all <- eventReactive(input$overlay_create_overlaid_models, { 
  # t.1 <- Sys.time() # For testing purposes
  #########################################################
  ### Reset/hide reactive values, preview plots, and eval metrics
  overlay_reset()
  
  #########################################################
  ### Model overlay prep
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
  ### Model overlay process
  withProgress(message = 'Model overlay step:', value = 0.1, {
    prog.total <- length(vals$models.ll) + 1
    
    #####################################
    ### Project model predictions as necessary
    # Polys and base projected in overlay_create_base_spdf() suite of funcs
    incProgress(0, detail = "Projecting model predictions")
    
    if (identical(overlay_crs(), crs.ll)) {
      models.to.overlay <- vals$models.ll[-base.idx]
    } else {
      models.to.overlay <- lapply(vals$models.orig[-base.idx], function(spdf) {
        # Project spdf if necessary
        if (identical(overlay_crs(), crs(spdf))) {
          spdf <- spdf
        } else {
          spdf <- spTransform(spdf, overlay_crs())
        }
        
        # Make spdf valid if it is not
        if (!suppressWarnings(gIsValid(spdf))) {
          spdf <- valid.poly(spdf, "Overlaid model predictions")
        }
        
        spdf
      })
    }
    
    
    #####################################
    ### Create base grid and spdf of model predictions used as grid
    incProgress(0.9 / prog.total, 
                detail = "Making base grid and overlaid model 1")
    
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
    vals$overlay.base.sp <- base.sp
    vals$overlay.base.specs <- base.specs
    
    # print("Creating/processing base grid took:"); print(Sys.time() - t.1)
    
    
    #####################################
    ### Create overlaid models
    # t.2 <- Sys.time()
    overlap.perc <- input$overlay_grid_coverage / 100
    models.overlaid.all <- mapply(function(model, prog.num) {
      incProgress(0.9 / prog.total,
                  detail = paste("Making overlaid model", prog.num))
      
      overlay.func(base.sp, model, overlap.perc)
    }, models.to.overlay, 2:(prog.total - 1), SIMPLIFY = FALSE)
    
    incProgress(0.9 / prog.total, detail = "Finishing overlay process")
    # print("Overlaying of models took:"); print(Sys.time() - t.2)
    
    
    #####################################
    ### Store overlaid models and their info
    # models.overlaid <- lapply(models.overlaid.all, function(m) m[[1]])
    models.overlaid <- models.overlaid.all
    models.overlaid <- append(models.overlaid, base.spdf, 
                              after = (base.idx - 1))
    
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
    
    
    #########################################################
    ### Ens.over prep
    overlay_ensemble_prep()
    
  })
  
  #########################################################
  # print("The entire overlay process took"); print(Sys.time() - t.1)
  
  if (all(c(gIsValid(base.spdf), gIsValid(base.sp), 
            sapply(models.overlaid, gIsValid)))) {
    "All model predictions overlaid successfully"
  } else {
    "Model predictions overlaid, but outputs invalid. Please restart eSDM."
  }
})

###############################################################################
