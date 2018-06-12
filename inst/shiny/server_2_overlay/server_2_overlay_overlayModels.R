### Overlay predictions that are already on the same grid...
### ...functionality is in 'server_2_overlay_overlayModels_samegrid.R'

### Get crs in which to do overlay
### Reset/hide reactive values, preview plots, and eval metrics
### Overlay models onto base grid and create vals$ens.over... objects


###############################################################################
### Where the overlay magic aka science happens

overlay_all <- eventReactive(input$overlay_create_overlaid_models, {
  #########################################################
  ### Reset/hide reactive values, preview plots, and eval metrics
  validate(
    need(overlay_reset(),
         "There was an error in the eSDM, please reload the tool")
  )

  #########################################################
  ### Model overlay prep
  # Get index of model predictions to be base grid
  base.idx <- overlay_base_idx()
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

  # Store base index and overlap percentage
  overlap.perc <- input$overlay_grid_coverage / 100
  vals$overlay.info <- list(base.idx, overlap.perc)

  # Get and store crs of coord system to be used in overlay process
  vals$overlay.crs <- overlay_crs()

  # Set flag for if all of the sdms have the same sfc geometry list column
  models.orig.sfc <- lapply(vals$models.orig, st_geometry)
  if (all(sapply(models.orig.sfc, identical, models.orig.sfc[[1]]))) {
    samegrid.flag <- TRUE
  } else {
    samegrid.flag <- FALSE
  }
  rm(models.orig.sfc)


  #########################################################
  ### Model overlay process
  withProgress(message = 'Model overlay step:', value = 0.1, {
    prog.total <- length(vals$models.ll) + 1

    #--------------------------------------------
    ### Project model predictions as necessary
    # Polys and base projected in overlay_create_base_sf() suite of reac funcs
    # Polygons have already been checked for if they're valid
    incProgress(0, detail = "Projecting model predictions if necessary")

    if (identical(overlay_crs(), crs.ll)) {
      models.preoverlay <- vals$models.ll[-base.idx]
    } else {
      models.preoverlay <- lapply(vals$models.orig[-base.idx], function(sdm) {
        if (!identical(overlay_crs(), st_crs(sdm))) {
          st_transform(sdm, overlay_crs())
        } else {
          sdm
        }
      })
    }


    #--------------------------------------------
    ### Create base grid (sfc) and overlaid model predictions (sf)
    incProgress(0.9 / prog.total, detail = paste(
      "Making the base grid and thus also overlaying original model", base.idx
    ))

    base.sf <- overlay_create_base_sf() %>%
      purrr::set_names(c("Pred.overlaid", "Weight.overlaid", "Pixels", "geometry"))

    base.sfc <- st_geometry(base.sf)

    # Get specs of base.sfc
    base.specs <- vals$models.specs[[base.idx]]
    base.specs[2] <- length(base.sfc) #length() for sfc object
    base.specs[3:4] <- NA
    if (identical(st_crs(base.sfc), crs.ll)) {
      base.sfc.ll <- base.sfc
    } else {
      base.sfc.ll <- st_transform(base.sfc, crs.ll)
    }
    base.specs[5] <- paste(
      as.character(round(st_bbox(base.sfc.ll)[c(1, 3, 2, 4)], 0)),
      collapse = ", "
    )
    rm(base.sfc.ll)

    # Store data
    vals$overlay.base.sfc <- base.sfc


    #--------------------------------------------
    ### Create overlaid models
    if (samegrid.flag) {
      # Same-grid overlay
      models.overlaid <- lapply(models.preoverlay, function(i, j) {
        incProgress(
          0.9 / prog.total,
          detail = "Overlaying original models using same-grid method"
        )
        sf.temp <- j %>%
          dplyr::left_join(st_set_geometry(i, NULL), by = "Pixels") %>%
          dplyr::select(Pred.overlaid = Pred, Weight.overlaid = Weight,
                        Pixels) %>%
          st_set_agr("constant")
      }, dplyr::select(base.sf, Pixels))

      temp.test <- sapply(models.overlaid, function(i) {
        identical(base.sfc, st_geometry(i))
      })
      validate(
        need(all(temp.test),
             "Error: the eSDM was unable to overlay the original models")
      )

    } else {
      # Standard overlay
      models.overlaid <- mapply(function(sdm, prog.num, sdm.num) {
        incProgress(
          0.9 / prog.total,
          detail = paste("Overlaying original model", sdm.num)
        )

        temp <- try( #overlay.sdm() crops 'sdm' to bbox of 'base.sfc'
          eSDM::overlay_sdm(base.sfc, sdm, overlap.perc, c("Pred", "Weight")),
          silent = TRUE
        )
        validate(
          need(temp,
               paste("Error: the eSDM was unable to overlay original model",
                     sdm.num))
        )

        df.toadd <- data.frame(Error.overlaid = NA, Pixels = 1:nrow(temp))
        temp %>%
          dplyr::bind_cols(df.toadd) %>%
          dplyr::select(c(1, 4, 2, 5, 3)) %>%
          st_set_agr("constant")
      },
      models.preoverlay, 2:(prog.total - 1),
      seq(1, length(vals$models.ll))[-base.idx], SIMPLIFY = FALSE)
    }

    incProgress(0.9 / prog.total, detail = "Finishing overlay process")


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
      list(c(base.specs[1], nrow(n), sum(!is.na(n$Pred.overlaid)),
             n.abund, base.specs[5]))
    }, models.overlaid.all, vals$models.pred.type)

    # Store info
    vals$overlaid.models <- models.overlaid.all
    vals$overlaid.models.specs <- specs.list


    #--------------------------------------------
    ### Ensemble prep
    list.null <- lapply(seq_along(vals$overlaid.models), function(d) NULL)
    vals$ens.over.wpoly.sf       <- list.null
    vals$ens.over.wpoly.filename <- list.null
    vals$ens.over.wpoly.coverage <- list.null
  })

  #########################################################

  # Do not need to test validity of any polygons because base polygon was
  # already checked and overlaid were made directly from base poly

  # No message so that nothing is left here if new environment is loaded
  ""
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
  vals$overlay.info          <- NULL
  vals$overlay.base.sfc      <- NULL
  vals$overlaid.models       <- list()
  vals$overlaid.models.specs <- NULL
  vals$overlaid.plot         <- NULL

  vals$ens.over.wpoly.filename <- NULL
  vals$ens.over.wpoly.sf       <- NULL
  vals$ens.over.wpoly.coverage <- NULL
  vals$ens.over.wpoly.plot     <- NULL

  vals$ensemble.models       <- list()
  vals$ensemble.method       <- NULL
  vals$ensemble.weights      <- NULL
  vals$ensemble.rescaling    <- NULL
  vals$ensemble.overlaid.idx <- NULL
  vals$ensemble.plot         <- NULL
  vals$ensemble.plot.idx  <- NULL

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
    browser()
    if (any(idx %in% vals$pretty.plotted.idx[[2]]) |
        any(idx %in% vals$pretty.plotted.idx[[3]])) {
      # shinyjs::hide("pretty_plot_plot", time = 0)
      vals$pretty.params.list <- NULL
      vals$pretty.plotted.idx <- NULL
    }
  }

  TRUE
})


###############################################################################
### Get crs object with projection to be used in overlay process
overlay_crs <- reactive({
  if (input$overlay_proj_native) {
    crs.selected <- st_crs(vals$models.orig[[overlay_base_idx()]])

  } else {
    crs.selected <- switch(
      as.numeric(input$overlay_proj_method),
      crs.ll,
      st_crs(vals$models.orig[[as.numeric(req(input$overlay_proj_sdm))]]),
      suppressWarnings(st_crs(input$overlay_proj_epsg))
    )
  }

  # Use [[2]] in case of custom crs w/out epsg code
  validate(
    need(isTruthy(crs.selected[[2]]),
         paste("Error: The entered EPSG code was not recognized,",
               "please enter a valid EPSG code"))
  )

  crs.selected
})


###############################################################################
### Get index of sdm to be used as base
overlay_base_idx <- reactive({
  as.numeric(input$overlay_loaded_table_rows_selected)
})


###############################################################################
