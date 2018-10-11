# Code for the overlay process and accompanying actions

###############################################################################
### Modal displayed when overlay button clicked
observeEvent(input$overlay_create_overlaid_models_modal, {
  showModal(modalDialog(
    title = "Do you want to save your workspace before overlaying?",
    tags$h5("The overlay process can take several minutes,",
            "and if you are not running the GUI locally the server might time out",
            "and you might lose your session progress (workspace).",
            "Thus, it is recommended that you save your workspace before overlaying"),

    footer = tagList(
      modalButton("Cancel overlay to save workspace"),
      actionButton("overlay_create_overlaid_models", "Proceed with overlay")
    )
  ))
})


###############################################################################
### Where the overlay magic aka science happens
overlay_all <- eventReactive(input$overlay_create_overlaid_models, {
  removeModal()

  #########################################################
  ### Reset/hide reactive values, preview plots, and eval metrics
  validate(
    need(overlay_reset(),
         "An error occurred; please restart the GUI and report an issue")
  )

  #########################################################
  ### Overlay prep
  # Get index of predictions to be base geometry
  base.idx <- overlay_base_idx()
  models.num <- length(vals$models.ll)

  validate(
    need(length(base.idx) == 1,
         paste("Error: Select exactly one set of predictions",
               "from the 'Imported Original Predictions' table",
               "to use as the base geometry")),
    need(models.num > 1,
         paste("Error: You must import more than one set of predictions",
               "into the GUI to overlay predictions")),
    if (input$overlay_bound) {
      need(vals$overlay.bound,
           paste("Error: Please either uncheck the 'study area polygon'",
                 "checkbox or import a study area polygon"))
    },
    if (input$overlay_land) {
      need(vals$overlay.land,
           paste("Error: Please either uncheck the 'erasing polygon'",
                 "checkbox or import an erasing polygon"))
    }
  )

  overlap.perc <- input$overlay_grid_coverage / 100


  #########################################################
  ### Overlay process
  withProgress(message = 'Overlay step:', value = 0.1, {
    prog.total <- length(vals$models.ll) + 1

    #--------------------------------------------
    ### Transform original predictions as necessary
    # Polys and base transformed in overlay_create_base_sf() suite of funcs
    # Polygons have already been checked for if they're valid
    incProgress(0, detail = "Projecting predictions if necessary")

    if (identical(overlay_crs(), crs.ll)) {
      models.preoverlay <- vals$models.ll[-base.idx]

    } else {
      models.preoverlay <- lapply(vals$models.orig[-base.idx], function(sdm) {
        st_transform(sdm, overlay_crs())
      })
    }


    #--------------------------------------------
    ### Create base geometry (base.sfc) and 1st overlaid predictions (base.sf)
    incProgress(0.9 / prog.total, detail = paste(
      "Making the base geometry and thus also overlaying Original", base.idx
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
    ### Check that all original predictions overlap with base.sfc
    base.sfc.union <- st_union(base.sfc)
    x <- sapply(models.preoverlay, function(i) {
      i <- suppressMessages(
        st_intersects(dplyr::filter(i, !is.na(Pred)), base.sfc)
      )
      any(sapply(i, length) > 0)
    })
    y <- which(!x)

    validate(
      need(all(x),
           paste("Error: The following set(s) of predictions do not have any",
                 "non-NA prediction polygons that overlap with",
                 paste0(
                   "the specified base geometry:\n",
                   paste("Original", ifelse(y >= base.idx, y + 1, y),
                         collapse = ", "))))
    )
    rm(base.sfc.union, x, y)


    #--------------------------------------------
    ### Create overlaid predictions
    base.pix <- dplyr::select(base.sf, Pixels)
    models.orig.sfc <- lapply(vals$models.orig, st_geometry)
    samegeo.flag <- sapply(
      models.orig.sfc[-base.idx], identical, models.orig.sfc[[base.idx]]
    )
    rm(models.orig.sfc)

    models.overlaid <- mapply(function(samegeo.flag.ind, sdm, sdm.num) {
      incProgress(
        0.9 / prog.total,
        detail = paste("Overlaying Original", sdm.num)
      )

      if (samegeo.flag.ind) {
        # SDM being overlaid has the SAME geometry as not-clipped or erased
        #   geometry of base.sfc
        #   If base.sfc is clipped geom of orig geom, then can index by Pixels
        sf.temp <- base.pix %>%
          dplyr::left_join(st_set_geometry(sdm, NULL), by = "Pixels") %>%
          dplyr::mutate(Pixels2 = 1:nrow(base.pix)) %>%
          dplyr::select(Pred.overlaid = Pred, Weight.overlaid = Weight,
                        Pixels = Pixels2) %>%
          st_set_agr("constant")

        validate(
          need(identical(base.sfc, st_geometry(sf.temp)),
               paste("Error: The GUI was unable to overlay Original",
                     sdm.num))
        )

        sf.temp

      } else {
        # SDM being overlaid has a DIFFERENT geometry than the base
        temp <- try( #overlay.sdm() crops 'sdm' to bbox of 'base.sfc'
          eSDM::overlay_sdm(base.sfc, sdm, c("Pred", "Weight"), overlap.perc),
          silent = TRUE
        )

        validate(
          need(temp,
               paste("Error: The GUI was unable to overlay Original",
                     sdm.num))
        )

        temp %>%
          st_set_geometry(NULL) %>%
          dplyr::bind_cols(Pixels = 1:nrow(temp)) %>%
          st_sf(geometry = st_geometry(temp), agr = "constant")
      }
    },
    samegeo.flag, models.preoverlay,
    seq(1, length(vals$models.ll))[-base.idx], SIMPLIFY = FALSE)

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
    specs.list <- mapply(function(n, p, idx) {
      if (p == 1) {
        n.abund <- unname(round(eSDM::model_abundance(n, "Pred.overlaid")))
      } else {
        n.abund <- "N/A"
      }
      list(c(as.character(table_orig()[idx, ]), base.specs[1], nrow(n),
             sum(!is.na(n$Pred.overlaid)), n.abund, base.specs[5]))
    }, models.overlaid.all, vals$models.pred.type, 1:models.num)


    #--------------------------------------------
    # Store overlaid info in vals
    # All storage done here so that all error checks happen before storage
    vals$overlay.base.sfc <- base.sfc
    vals$overlay.crs <- overlay_crs()
    vals$overlay.info <- list(
      vals$models.names[base.idx], overlay_studyarea_land_message(),
      overlay_crs_message(), overlap.perc
    )

    vals$overlaid.models <- models.overlaid.all
    vals$overlaid.models.specs <- specs.list


    #--------------------------------------------
    ### Ensemble prep
    list.null <- vector("list", length(vals$overlaid.models))
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
### Reset applicable vals elements before creating new overlaid objects
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

  vals$ensemble.models        <- list()
  vals$ensemble.method        <- NULL
  vals$ensemble.weights       <- NULL
  vals$ensemble.rescaling     <- NULL
  vals$ensemble.overlaid.idx  <- NULL
  vals$ensemble.plot.leaf     <- NULL
  vals$ensemble.plot.leaf.idx <- NULL
  vals$ensemble.plot          <- NULL
  vals$ensemble.plot.idx      <- NULL

  # TODO: Could make this so it only removes overlaid metrics
  if (isTruthy(vals$eval.models.idx)) {
    if (isTruthy(vals$eval.models.idx[[2]])){
      vals$eval.models.idx    <- NULL
      vals$eval.metrics       <- NULL
      vals$eval.metrics.names <- NULL
    }
  }

  TRUE
}


###############################################################################
### Get crs to be used in overlay process
overlay_crs <- reactive({
  if (input$overlay_proj_native) {
    crs.selected <- st_crs(vals$models.orig[[overlay_base_idx()]])

  } else {
    crs.selected <- switch(
      as.numeric(input$overlay_proj_method), crs.ll,
      st_crs(vals$models.orig[[as.numeric(req(input$overlay_proj_sdm))]]),
      suppressWarnings(st_crs(input$overlay_proj_epsg))
    )
  }

  validate(
    need(isTruthy(crs.selected$proj4string),
         paste("Error: The entered EPSG code was not recognized,",
               "please enter a valid EPSG code"))
  )

  crs.selected
})


### Generate crs message to provide info about overlaid predictions
overlay_crs_message <- reactive({
  req(overlay_crs())

  if (input$overlay_proj_native) {
    "In the native coordinate system of the base geometry"

  } else {
    switch(
      as.numeric(input$overlay_proj_method),
      "In WGS 84 geographic coordinates",
      paste(
        "In the coordinate system of the",
        paste0(
          "'", vals$models.names[as.numeric(req(input$overlay_proj_sdm))], "'"
        ), "SDM"
      ),
      paste("In the coordinate system of EPSG code", input$overlay_proj_epsg)
    )
  }
})


###############################################################################
### Get index of predictions to be used as base geometry
overlay_base_idx <- reactive({
  as.numeric(input$overlay_loaded_table_rows_selected)
})


###############################################################################
### Generate message about study area and/or erasing polygons used
overlay_studyarea_land_message <- reactive({
  if (isTruthy(vals$overlay.land) & isTruthy(vals$overlay.bound)) {
    "Both a study area polygon and an erasing polygon were used"

  } else if (isTruthy(vals$overlay.bound)) {
    "Only a study area polygon was used"

  } else if (isTruthy(vals$overlay.land)) {
    "Only an erasing polygon was used"

  } else {
    "Neither a study area polygon nor an erasing polygon were used"
  }
})

###############################################################################
