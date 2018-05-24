### Overlay predictions that are already on the same grid...
### ...functionality is in 'server_2_overlay_overlayModels_samegrid.R'

### Get crs in which to do overlay
### Reset/hide reactive values, preview plots, and eval metrics
### Overlay models onto base grid and create vals$ens.over... objects


###############################################################################
### Where the overlay magic aka science happens

overlay_all <- eventReactive(input$overlay_create_overlaid_models, {
  # t.1 <- Sys.time() # For testing purposes
  #########################################################
  ### Reset/hide reactive values, preview plots, and eval metrics
  validate(
    need(overlay_reset(),
         "There was an error in the eSDM, please reload the tool")
  )

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

    #--------------------------------------------
    ### Project model predictions as necessary
    # Polys and base projected in overlay_create_base_sf() suite of reac funcs
    # Polygons have already been checked for if they're valid
    incProgress(0, detail = "Projecting model predictions")

    if (identical(overlay_crs(), crs.ll)) {
      models.preoverlay <- vals$models.ll[-base.idx]
    } else {
      models.preoverlay <- lapply(vals$models.orig[-base.idx], function(sf.obj) {
        if (!identical(overlay_crs(), st_crs(sf.obj))) {
          st_transform(sf.obj, overlay_crs())
        } else {
          sf.obj
        }
      })
    }


    #--------------------------------------------
    ### Create base grid and spdf of model predictions used as grid
    incProgress(0.9 / prog.total,
                detail = "Making base grid and overlaid model 1")

    base.sf <- overlay_create_base_sf()
    # browser()
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

    # Store data
    vals$overlay.base.sfc <- base.sfc
    vals$overlay.base.specs <- base.specs

    # print("Creating/processing base grid took:"); print(Sys.time() - t.1)


    #--------------------------------------------
    ### Create overlaid models
    # t.2 <- Sys.time()
    overlap.perc <- input$overlay_grid_coverage / 100

    models.overlaid <- mapply(function(model, prog.num) {
      incProgress(0.9 / prog.total,
                  detail = paste("Making overlaid model", prog.num))
      # overlay.sdm() crops 'model' to bbox of 'base.sfc'
      temp <- try(
        eSDM::overlay_sdm(base.sfc, model, overlap.perc, c("Pred", "Weight")),
        silent = TRUE
      )

      validate(
        need(isTruthy(temp),
             paste("Error: the eSDM was unable to overlay model", prog.num))
      )

      df.toadd <- data.frame(Error.overlaid = NA, Pixels = 1:nrow(temp))
      temp %>%
        dplyr::bind_cols(df.toadd) %>%
        dplyr::select(c(1, 4, 2, 5, 3)) %>%
        st_set_agr("constant")
    }, models.preoverlay, 2:(prog.total - 1), SIMPLIFY = FALSE)

    incProgress(0.9 / prog.total, detail = "Finishing overlay process")
    # print("Overlaying of models took:"); print(Sys.time() - t.2)


    #--------------------------------------------
    ### Procces then store overlaid models and their info in vals
    models.overlaid.all <- c(models.overlaid, list(base.sf))

    # Reorder model list
    temp <- length(models.overlaid.all)
    if (base.idx == 1 & temp == 1) {
      models.order <- 1
    } else if (base.idx == 1) {
      models.order <- c(temp, base.idx:(temp - 1))
    } else if (base.idx == temp) {
      models.order <- 1:temp
    } else {
      models.order <- c(1:(base.idx - 1), temp, base.idx:(temp - 1))
    }
    rm(temp)
    models.overlaid.all <- models.overlaid.all[models.order]

    # Get model specs
    specs.list <- mapply(function(n, p) {
      if (p == 1) {
        n.abund <- unname(round(model_abundance(n, "Pred.overlaid")))
      } else {
        n.abund <- "N/A"
      }
      list(c(base.specs[1], length(n), sum(!is.na(n$Pred.overlaid)),
             n.abund, base.specs[5]))
    }, models.overlaid.all, vals$models.pred.type)

    # Store info
    vals$overlaid.models <- models.overlaid.all
    vals$overlaid.models.specs <- specs.list


    #--------------------------------------------
    ### Ensemble prep
    # overlay_ensemble_prep()
    list.null <- lapply(seq_along(vals$overlaid.models), function(d) NULL)
    vals$ens.over.wpoly.sf       <- list.null
    vals$ens.over.wpoly.filename <- list.null
    vals$ens.over.wpoly.coverage <- list.null
  })

  #########################################################

  # Do not need to test validity of any polygons because base polygon was
  # already checked and overlaid were made directly from base poly
  # print("The entire overlay process took"); print(Sys.time() - t.1)
  "All model predictions overlaid successfully"
})


###############################################################################
#------------------------------------------------------------------------------
###############################################################################


###############################################################################
### Reset vals$overlaid..., vals.ens.over..., vals$ensemble...,
# and vals$eval... (if any overlaid metrics are calc'd)
# before creating new overlaid
### Called in overlay_all() and overlay_samegrid_all()
overlay_reset <- reactive({
  vals$overlay.crs           <- NULL
  vals$overlay.base.idx      <- NULL
  vals$overlay.base.sfc      <- NULL
  vals$overlay.base.specs    <- NULL
  vals$overlaid.models       <- list()
  vals$overlaid.models.specs <- NULL

  vals$ens.over.wpoly.filename <- NULL
  vals$ens.over.wpoly.sf       <- NULL
  vals$ens.over.wpoly.coverage <- NULL

  vals$ensemble.models       <- list()
  vals$ensemble.method       <- NULL
  vals$ensemble.weights      <- NULL
  vals$ensemble.rescaling    <- NULL
  vals$ensemble.overlaid.idx <- NULL
  vals$ensemble.plotted.idx  <- NULL

  # Could make this so it only removes overlaid metrics
  # TODO: make smarter
  if (!is.null(vals$eval.models.idx)) {
    if (!is.null(vals$eval.models.idx[[2]])){
      vals$eval.models.idx    <- NULL
      vals$eval.metrics       <- NULL
      vals$eval.metrics.names <- NULL
    }
  }

  # Reset pretty vals if an overlaid or ensemble model was plotted
  if (!is.null(vals$pretty.plotted.idx)) {
    if (any(idx %in% vals$pretty.plotted.idx[[2]]) |
        any(idx %in% vals$pretty.plotted.idx[[3]])) {
      shinyjs::hide("pretty_plot_plot", time = 0)
      vals$pretty.params.list <- NULL
      vals$pretty.plotted.idx <- NULL
    }
  }

  # Hide plot windows: this is done in server_hide_show.R

  TRUE
})


###############################################################################
### Get crs object with projection to be used in overlay process
overlay_crs <- reactive({
  if (input$overlay_samegrid_indicator == 2) {
    model.idx <- as.numeric(input$overlay_loaded_table_rows_selected)
    st_crs(vals$models.orig[[model.idx]])

  } else {
    if (input$overlay_proj_ll) {
      crs.ll
    } else {
      if (input$overlay_proj_opt == 1) {
        st_crs(vals$models.orig[[as.numeric(input$overlay_proj_which)]])

      } else {
        x <- suppressWarnings(st_crs(input$overlay_proj_epsg))
        validate(
          need(!is.na(x$epsg),
               paste("Error: The entered EPSG code was not recognized,",
                     "please enter a valid EPSG code"))
        )
        x
      }
    }
  }
})

###############################################################################
