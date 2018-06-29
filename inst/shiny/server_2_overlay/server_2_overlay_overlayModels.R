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
  # Get index of model predictions to be base geometry
  base.idx <- overlay_base_idx()
  models.num <- length(vals$models.ll)

  validate(
    need(length(base.idx) == 1,
         paste("Error: Please select exactly one model from the table",
               "to use as the base geometry")),
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

  overlap.perc <- input$overlay_grid_coverage / 100

  # Set flag for if all of the sdms have the same sfc geometry list column
  models.orig.sfc <- lapply(vals$models.orig, st_geometry)
  samegeo.flag <- all(sapply(models.orig.sfc, identical, models.orig.sfc[[1]]))
  rm(models.orig.sfc)


  #########################################################
  ### Model overlay process
  withProgress(message = 'Model overlay step:', value = 0.1, {
    prog.total <- length(vals$models.ll) + 1

    #--------------------------------------------
    ### Transform model predictions as necessary
    # Polys and base transformed in overlay_create_base_sf() suite of reac funcs
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
    ### Create base geometry (base.sfc) and 1st overlaid model predictions (base.sf)
    incProgress(0.9 / prog.total, detail = paste(
      "Making the base geometry and thus also overlaying original model", base.idx
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


    #--------------------------------------------
    ### Check that all original models overlap with base.sfc
    base.sfc.union <- st_union(base.sfc)
    x <- sapply(models.preoverlay, function(i) {
      any(sapply(suppressMessages(st_intersects(i, base.sfc)), length) > 0)
    })

    validate(
      need(all(x),
           paste("Error: The following sets of predictions do not overlap",
                 "with the specified base geometry:\n",
                 paste("Original", which(!x), collapse = ", ")))
    )
    rm(base.sfc.union, x)


    #--------------------------------------------
    ### Create overlaid models
    if (samegeo.flag) {
      # Same-geometry overlay
      models.overlaid <- lapply(models.preoverlay, function(i, j) {
        incProgress(
          0.9 / prog.total,
          detail = "Overlaying original models"
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

        temp %>%
          dplyr::bind_cols(Pixels = 1:nrow(temp)) %>%
          dplyr::select(c(1, 2, 4, 3)) %>%
          st_set_agr("constant")
      },
      models.preoverlay, 2:(prog.total - 1),
      seq(1, length(vals$models.ll))[-base.idx], SIMPLIFY = FALSE)
    }

    incProgress(0.9 / prog.total, detail = "Finishing overlay process")


    #--------------------------------------------
    ### Procces overlaid models and their info
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


    #--------------------------------------------
    # Store overlaid info in vals
    # All done here so that all error checks happen before storage
    vals$overlay.base.sfc <- base.sfc
    vals$overlay.crs <- overlay_crs()
    vals$overlay.info <- list(base.idx, overlay_studyarea_land_message(),
                              overlay_crs_message(), overlap.perc)

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
### Reset  applicable vals elements before creating new overlaid things
### Is a function rather than reactive so that it runs every time
overlay_reset <- function() {
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
}


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


### Generate crs message to provide info about overlaid models
overlay_crs_message <- reactive({
  req(overlay_crs())

  if (input$overlay_proj_native) {
    "In the native coordiante system of the base geometry"

  } else {
    switch(
      as.numeric(input$overlay_proj_method),
      "In WGS 84 geographic coordinates",
      paste("In the coordinate system of the",
            paste0("'", vals$models.names[as.numeric(req(input$overlay_proj_sdm))], "'"),
            "SDM"),
      paste("In the coordinate system of EPSG code", input$overlay_proj_epsg)
    )
  }
})


###############################################################################
### Get index of sdm to be used as base
overlay_base_idx <- reactive({
  as.numeric(input$overlay_loaded_table_rows_selected)
})


###############################################################################
overlay_studyarea_land_message <- reactive({
  if (isTruthy(vals$overlay.land) & isTruthy(vals$overlay.bound)) {
    "Both a study area polygon and a land area polygon were used"

  } else if (isTruthy(vals$overlay.bound)) {
    "Only a study area area polygon was used"

  } else if (isTruthy(vals$overlay.land)) {
    "Only a land area polygon was used"

  } else {
    "Neither a land area polygon and a study area polygon were used"
  }
})


###############################################################################
