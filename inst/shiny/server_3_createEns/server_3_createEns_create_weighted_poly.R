### Code for creating weighted ensembles using method 4: weights from polygons


###############################################################################
### Flag for is any weight polygons are loaded
output$create_ens_weighted_poly_flag <- reactive({
  any(sapply(vals$ens.over.wpoly.filename, isTruthy))
})
outputOptions(output, "create_ens_weighted_poly_flag", suspendWhenHidden = FALSE)


###############################################################################
### Create weighted ensemble using polygon weights
create_ens_weighted_poly <- reactive({
  validate(
    need(sum(!sapply(vals$ens.over.wpoly.filename, is.null)) > 0,
         paste("Error: Please load at least one weight polygon in order to",
               "use this weighted ensembling method"))
  )

  data.rescaled <- create_ens_data_rescale()
  base.sfc <- vals$overlay.base.sfc
  data.weights <- create_ens_weights_poly_weights()

  data.reweighted <- data.rescaled * data.weights
  data.ens <- data.frame(
    Pred.ens = apply(data.reweighted, 1, mean, na.rm = TRUE)
  )

  st_sf(data.ens, geometry = base.sfc, agr = "constant")
})

### Get weights based on loaded polygons
create_ens_weights_poly_weights <- reactive({
  idx <- create_ens_overlaid_idx()

  x <- as.data.frame(
    mapply(function(pred.sf, wpoly.sf.list, wpoly.coverage.vec) {
      if (is.null(wpoly.sf.list)) {
        # If overlaid model has no weight polys, predictions have weight of 1
        rep(1, nrow(pred.sf))

      } else {
        # Else, return vector with weights
        w.list <- mapply(function(wpoly.sf, wpoly.coverage) {
          poly_weight(pred.sf, wpoly.sf, wpoly.coverage)
        }, wpoly.sf.list, wpoly.coverage.vec, SIMPLIFY = FALSE)

        w <- rep(1, nrow(pred.sf))
        for(i in w.list) {
          i.idx <- i[[1]]
          i.weight <- i[[2]]
          w[i.idx] <- i.weight
        }
        w
      }
    },
    vals$overlaid.models[idx], vals$ens.over.wpoly.sf[idx],
    vals$ens.over.wpoly.coverage[idx],
    SIMPLIFY = FALSE)
  )
  names(x) <- letters[1:length(x)]

  x
})


### Function for weighting overlapping areas by weight in weight poly
# Returns list of indices of preds to be weighted and weight for those preds
poly_weight <- function(poly.pred, poly.w, coverage) {
  stopifnot(
    inherits(poly.pred, "sf") & inherits(poly.w, "sf"),
    length(unique(poly.w$Weight)) == 1
  )

  poly.w.union <- st_sf(
    Weight = unique(poly.w$Weight), geometry = st_union(poly.w),
    agr = "constant"
  )
  y <- suppressMessages(st_intersection(poly.pred, poly.w.union))

  area.ratio <- as.numeric(st_area(y) / st_area(poly.pred)[y$Pixels])
  idx.toweight <- y$Pixels[area.ratio >= (coverage / 100)]

  list(idx.toweight, unique(poly.w$Weight))
}


###############################################################################
### Plot preview of weight polygons
create_ens_weights_poly_preview <- eventReactive(
  input$create_ens_weights_poly_preview_execute, {
    req(vals$ens.over.wpoly.filename)
    overlaid.which <- as.numeric(input$create_ens_weights_poly_preview_model)

    validate(
      need(isTruthy(vals$ens.over.wpoly.sf[[overlaid.which]]),
           paste("Error: This overlaid model does not have any",
                 "assigned weight polygons to preview"))
    )

    plot(st_geometry(vals$overlaid.models[[overlaid.which]]), axes = TRUE,
         col = "black", border = NA)

    for(sf.toplot in vals$ens.over.wpoly.sf[[overlaid.which]]) {
      plot(st_geometry(sf.toplot), add = TRUE, col = NA, border = "red")
    }
  }
)


###############################################################################
### Remove loaded weight polygons
create_ens_weights_poly_remove <- eventReactive(
  input$create_ens_weights_poly_remove_execute, {
    validate(
      need(!is.null(input$create_ens_weights_poly_remove_choices),
           "Error: Please select at least one weighted polygon to remove")
    )

    # Get indices of wpoly objects to remove
    poly.toremove.idx <- input$create_ens_weights_poly_remove_choices
    poly.toremove.idx <- lapply(strsplit(poly.toremove.idx, ", "), as.numeric)
    poly.toremove.df  <- data.frame(t(data.frame(poly.toremove.idx)))

    poly.toremove.df.list <- by(poly.toremove.df, poly.toremove.df[, 1],
                                function(j) c(j[, 2]))

    # Generate 3 element list of vectors of wpoly objects to remove
    poly.toremove.list <- lapply(1:3, function(i) {
      if (i %in% names(poly.toremove.df.list)) {
        poly.toremove.df.list[[as.character(i)]]
      } else {
        NULL
      }
    })

    # Remove selected wpoly objects from vals
    idx.model <- 0
    for(idx.poly in poly.toremove.list) {
      idx.model <- idx.model + 1

      if (!is.null(idx.poly)) {
        x <- vals$ens.over.wpoly.filename[[idx.model]][-idx.poly]
        y <- vals$ens.over.wpoly.sf[[idx.model]][-idx.poly]
        z <- vals$ens.over.wpoly.coverage[[idx.model]][-idx.poly]

        if (length(x) == 0) {
          vals$ens.over.wpoly.filename[idx.model] <- list(NULL)
          vals$ens.over.wpoly.sf[idx.model]       <- list(NULL)
          vals$ens.over.wpoly.coverage[idx.model] <- list(NULL)
        } else {
          vals$ens.over.wpoly.filename[[idx.model]] <- x
          vals$ens.over.wpoly.sf[[idx.model]]       <- y
          vals$ens.over.wpoly.coverage[[idx.model]] <- z
        }
      }
    }

    "Weighted poly removed"
  }
)


###############################################################################
### Table summarizing loaded polygon weights
create_ens_weights_poly_table <- reactive({
  req(vals$ens.over.wpoly.filename)

  models.which <- seq_along(vals$overlaid.models)
  if (input$create_ens_table_subset) {
    ens.selected <- input$create_ens_datatable_rows_selected
    models.which <- models.which[models.which %in% ens.selected]
  }

  req(length(models.which) >= 2) # validate()'s done elsewhere

  overlaid.names <- paste("Overlaid", models.which)

  if (all(sapply(vals$ens.over.wpoly.filename[models.which], is.null))) {
    overlaid.filenames <- ""
    overlaid.weights   <- ""
    overlaid.coverage  <- ""

  } else {
    overlaid.filenames <- sapply(vals$ens.over.wpoly.filename,
                                 paste, collapse = ", ")[models.which]

    overlaid.weights <- sapply(vals$ens.over.wpoly.sf, function(i) {
      paste(lapply(i, function(j) {
        ifelse(length(unique(j$Weight)) > 1, "Multiple", j$Weight[1])
      }), collapse = ", ")
    })[models.which]

    overlaid.coverage <- sapply(
      vals$ens.over.wpoly.coverage, paste, collapse = ", "
    )[models.which]
  }

  table.out <- data.frame(overlaid.names, overlaid.filenames,
                          overlaid.weights, overlaid.coverage,
                          stringsAsFactors = FALSE)
  names(table.out) <- c("Predictions", "File(s)", "Weight(s)", "Coverage(s)")

  table.out
})


###############################################################################
# Add filename, weighted polygon, and coverage percentage to
#   vals$ens.over.wpoly... objects
create_ens_weights_poly_add <- eventReactive(
  input$create_ens_weights_poly_add_execute,
  {
    validate(
      need(!is.null(input$create_ens_weights_poly_model),
           paste("Error: Please select at least one overlaid model",
                 "to which to apply weight polygon"))
    )

    ### Get input information
    overlaid.list <- strsplit(input$create_ens_weights_poly_model, " ")
    overlaid.selected <- sapply(overlaid.list, function(i) as.numeric(i[[2]]))

    poly.filetype <- as.numeric(input$create_ens_weights_poly_type)
    poly.filetype.txt <- switch(poly.filetype, "CSV", "Raster", "SHP", "GDB")


    ### Get/process weight polygon based on filetype
    if (poly.filetype == 1) {
      # .csv filetype
      poly.filename <- create_ens_weights_poly_csv_process()[[2]]
      poly.sfc      <- create_ens_weights_poly_csv_process()[[1]]
      poly.sf       <- st_sf(Weight = input$create_ens_weights_poly_csv_weight,
                             geometry = poly.sfc)
      rm(poly.sfc)

    } else if (poly.filetype == 2) {
      # .tif filetype
      poly.filename <- create_ens_weights_poly_raster_read()[[2]]
      poly.sf       <- create_ens_weights_poly_raster_read()[[1]]

      names(poly.sf)[1] <- "Weight"
      if (input$create_ens_weights_poly_raster_weight_type == 1) {
        poly.sf$Weight <- input$create_ens_weights_poly_raster_weight
      }

    } else if (poly.filetype == 3) {
      # .shp filetype
      poly.filename <- create_ens_weights_poly_shp_read()[[2]]
      poly.sf       <- create_ens_weights_poly_shp_read()[[1]]

      if (input$create_ens_weights_poly_shp_weight_type == 1) {
        poly.sf <- st_as_sf(Weight = input$create_ens_weights_poly_shp_weight,
                            geometry = st_geometry(poly.sf))

      } else {
        poly.sf <- poly.sf[input$create_ens_weights_poly_shp_field]
        names(poly.sf[1]) <- "Weight"
      }

    } else if (poly.filetype == 4) {
      # .gdb filetype
      poly.filename <- create_ens_weights_poly_gdb_read()[[2]]
      poly.sf       <- create_ens_weights_poly_gdb_read()[[1]]

      if (input$create_ens_weights_poly_gdb_weight_type == 1) {
        poly.sf <- st_as_sf(Weight = input$create_ens_weights_poly_gdb_weight,
                            geometry = st_geometry(poly.sf))

      } else {
        poly.sf <- poly.sf[input$create_ens_weights_poly_gdb_field]
        names(poly.sf)[1] <- "Weight"
      }

    } else {
      validate(
        need(FALSE, "Error: create_ens_weights_poly_add() filetype error")
      )
    }

    stopifnot(inherits(poly.sf, "sf"), is.numeric(overlaid.selected))
    st_agr(poly.sf) <- "constant"


    ### Ensure that weight polygon has same crs as overlaid models
    if (!identical(st_crs(poly.sf), vals$overlay.crs)) {
      poly.sf <- st_transform(poly.sf, vals$overlay.crs)
    }

    ### Make sure that new polygon doesn't overlap with loaded polygons
    ###  already assigned to same overlaid model
    sapply(overlaid.selected, function(overlaid.idx) {
      if (!is.null(vals$ens.over.wpoly.sf[[overlaid.idx]])) {
        mapply(function(poly.loaded, poly.idx) {
          x <- suppressMessages(st_intersection(poly.sf, poly.loaded))
          validate(
            need(nrow(x) == 0,
                 paste("Error: Cannot load weight polygon because",
                       "polygon overlaps with weight polygon number",
                       poly.idx, "of overlaid model", overlaid.idx))
          )
        },
        vals$ens.over.wpoly.sf[[overlaid.idx]],
        seq_along(vals$ens.over.wpoly.sf[[overlaid.idx]]))
      }
    })

    ### Add poly.sf and coverage val to applicable indices of vals$ens...
    ewf.selected     <- vals$ens.over.wpoly.filename[overlaid.selected]
    ewf.selected.new <- lapply(ewf.selected, function(l) c(l, poly.filename))
    vals$ens.over.wpoly.filename[overlaid.selected] <- ewf.selected.new

    ews.selected     <- vals$ens.over.wpoly.sf[overlaid.selected]
    ews.selected.new <- lapply(ews.selected, function(l) c(l, list(poly.sf)))
    vals$ens.over.wpoly.sf[overlaid.selected] <- ews.selected.new

    poly.coverage    <- input$create_ens_weights_poly_coverage
    ewc.selected     <- vals$ens.over.wpoly.coverage[overlaid.selected]
    ewc.selected.new <- lapply(ewc.selected, function(l) c(l, poly.coverage))
    vals$ens.over.wpoly.coverage[overlaid.selected] <- ewc.selected.new

    ### Output message
    paste(poly.filetype.txt, "weight polygon added as weight for:",
          paste(input$create_ens_weights_poly_model, collapse = ", "))
  }
)


###############################################################################
# File-specific loading and processing to sf object, and their respective flags

#----------------------------------------------------------
# CSV

### Flag for successfully loaded file
output$create_ens_weights_poly_csv_flag <- reactive({
  !is.null(create_ens_weights_poly_csv_read())
})
outputOptions(output, "create_ens_weights_poly_csv_flag",
              suspendWhenHidden = FALSE)

### Load and process
create_ens_weights_poly_csv_read <- reactive({
  file.in <- input$create_ens_weights_poly_csv_file
  req(file.in)

  # Ensure file extension is .csv (RStudio type, browser type)
  if (!(file.in$type %in% c("text/csv", "application/vnd.ms-excel"))) return()

  csv.data <- read.csv(file.in$datapath, stringsAsFactors = FALSE)

  return(list(file.in$name, csv.data))
})

create_ens_weights_poly_csv_process <- reactive({
  withProgress(message = 'Loading csv polygon', value = 0.6, {
    csv.poly.list <- create_ens_weights_poly_csv_read()
    csv.poly.filename <- csv.poly.list[[1]]
    csv.poly.data <- csv.poly.list[[2]]
    csv.poly.data[csv.poly.data == ""] <- NA

    csv.poly.sfc <- create_sfc_csv_func(csv.poly.data, crs.ll)
    incProgress(0.3)

    # Check that polygon(s) are valid
    csv.poly.sfc <- check_valid(csv.poly.sfc, progress.detail = TRUE)
    incProgress(0.1)
  })

  list(csv.poly.sfc, csv.poly.filename)
})


#----------------------------------------------------------
# GIS raster

### Flag for successfully loaded file
output$create_ens_weights_poly_raster_flag <- reactive({
  !is.null(create_ens_weights_poly_raster_read())
})
outputOptions(output, "create_ens_weights_poly_raster_flag",
              suspendWhenHidden = FALSE)


### Load and process
create_ens_weights_poly_raster_read <- reactive({
  file.in <- input$create_ens_weights_poly_raster_file
  req(file.in)

  # Ensure file extension is .tif
  if (file.in$type != "image/tiff") return()


  withProgress(message = "Loading GIS raster", value = 0.2, {
    raster.band.num <- ifelse(
      input$create_ens_weights_poly_raster_weight_type == 1,
      1, input$create_ens_weights_poly_raster_band
    )
    gis.file.raster <- try(raster(file.in$datapath, band = raster.band.num),
                           silent = TRUE)
    gis.file.success <- isTruthy(gis.file.raster)
    incProgress(0.4)

    # If specified file could be loaded as a raster, process raster
    if (gis.file.success) {
      gis.file.raster <- st_as_sf(as(gis.file.raster, "SpatialPolygonsDataF"))
      # TODO change ^ to as(x, "SpatialPolygons")?
      st_agr(gis.file.raster) <- "constant"
      stopifnot(ncol(gis.file.raster) == 2)
      incProgress(0.1)

      # Adjust 0 - 360 data to -180 - 180 if needed
      gis.file.raster <- check_dateline(gis.file.raster, 60)
      incProgress(0.1)

      # Check that polygon(s) are valid
      gis.file.raster <- check_valid(gis.file.raster, progress.detail = TRUE)
      incProgress(0.1)
    }
  })

  if(!gis.file.success) {
    NULL
  } else {
    list(weight.raster, file.in$name)
  }
})


#----------------------------------------------------------
# GIS shp

### Flag for successfully loaded file
output$create_ens_weights_poly_shp_flag <- reactive({
  !is.null(create_ens_weights_poly_shp_read())
})
outputOptions(
  output, "create_ens_weights_poly_shp_flag", suspendWhenHidden = FALSE
)

### Load and process
create_ens_weights_poly_shp_read <- reactive({
  files.in <- input$create_ens_weights_poly_shp_files
  req(files.in)

  withProgress(message = "Loading GIS shapefile", value = 0.3, {
    gis.file.shp <- read.shp.shiny(files.in)
    incProgress(0.4)

    gis.file.success <- isTruthy(gis.file.shp)

    if (gis.file.success) {
      # Make sf object if necessary
      if (inherits(gis.file.shp, "sfc")) {
        gis.file.shp <- st_sf(Weight = NA, geometry = gis.file.shp)
      }
      incProgress(0.1)

      # Adjust 0 - 360 data to -180 - 180 if needed
      gis.file.shp <- check_dateline(gis.file.shp, 60)
      incProgress(0.1)

      # Check that polygon(s) are valid
      gis.file.shp <- check_valid(gis.file.shp, progress.detail = TRUE)
      incProgress(0.1)
    }
  })

  if(!gis.file.success) {
    NULL
  } else {
    list(gis.file.shp, strsplit(files.in$name[1], "[.]")[[1]][1])
  }
})


#----------------------------------------------------------
# GIS gdb

### Flag for successfully loaded file
output$create_ens_weights_poly_gdb_flag <- reactive({
  !is.null(create_ens_weights_poly_gdb_read())
})
outputOptions(output, "create_ens_weights_poly_gdb_flag", suspendWhenHidden = FALSE)


### Load and process
create_ens_weights_poly_gdb_read <- eventReactive(
  input$create_ens_weights_poly_gdb_load,
  {
    gdb.path <- input$create_ens_weights_poly_gdb_path
    gdb.name <- input$create_ens_weights_poly_gdb_name

    withProgress(message = "Loading GIS .gdb file", value = 0.3, {
      gis.file.gdb <- try(st_read(gdb.path, gdb.name, quiet = TRUE),
                          silent = TRUE)
      incProgress(0.4)

      gis.file.success <- isTruthy(gis.file.gdb)
      if (gis.file.success) {
        # Make sf object if necessary
        if (inherits(gis.file.gdb, "sfc")) {
          gis.file.gdb <- st_sf(Weight = NA, geometry = gis.file.gdb)
        }
        incProgress(0.1)

        # Adjust 0 - 360 data to -180 - 180 if needed
        gis.file.gdb <- check_dateline(gis.file.gdb, 60)
        incProgress(0.1)

        # Check that polygon(s) are valid
        gis.file.gdb <- check_valid(gis.file.gdb, progress.detail = TRUE)
        incProgress(0.1)
      }
    })

    if(!gis.file.success) {
      NULL
    } else {
      list(gis.file.gdb, gdb.name)
    }
  })


###############################################################################
