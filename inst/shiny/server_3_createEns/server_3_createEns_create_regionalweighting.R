### Code for creating weighted ensembles using method 4: weights from polygons


###############################################################################
### Flag for is any weight polygons are loaded
output$create_ens_weighted_poly_flag <- reactive({
  any(sapply(vals$ens.over.wpoly.filename, isTruthy))
})
outputOptions(output, "create_ens_weighted_poly_flag", suspendWhenHidden = FALSE)


###############################################################################
### create_ens_data_reg() is in '...createEns_create.R'

### Get weights based on loaded polygons
create_ens_reg_weights <- reactive({
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

  purrr::set_names(x, letters[1:length(x)])
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
# Code located in 'server_plots.R'


###############################################################################
### Remove loaded weight polygons
create_ens_reg_remove <- eventReactive(
  input$create_ens_reg_remove_execute, {
    validate(
      need(!is.null(input$create_ens_reg_remove_choices),
           "Error: Please select at least one weighted polygon to remove")
    )

    # Get indices of wpoly objects to remove
    poly.toremove.idx <- input$create_ens_reg_remove_choices
    poly.toremove.idx <- lapply(strsplit(poly.toremove.idx, ", "), as.numeric)
    poly.toremove.df  <- data.frame(t(data.frame(poly.toremove.idx)))

    poly.toremove.df.list <- by(
      poly.toremove.df, poly.toremove.df[, 1], function(j) c(j[, 2])
    )

    # Generate 3 element list of vectors of wpoly objects to remove
    x <- seq_along(vals$ens.over.wpoly.filename)
    poly.toremove.list <- lapply(x, function(i) {
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

    vals$ens.over.wpoly.plot <- NULL

    "Weighted poly removed"
  }
)


###############################################################################
### Table summarizing overlaid predicitons and their assigned weight polygons
create_ens_reg_table <- reactive({
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

  data.frame(overlaid.names, overlaid.filenames, overlaid.weights,
             overlaid.coverage, stringsAsFactors = FALSE) %>%
    purrr::set_names(c("Predictions", "File(s)", "Weight(s)", "Coverage(s)"))
})


###############################################################################
# Add filename, weighted polygon, and coverage percentage to
#   vals$ens.over.wpoly... objects
create_ens_reg_add <- eventReactive(
  input$create_ens_reg_add_execute,
  {
    validate(
      need(input$create_ens_reg_model,
           paste("Error: Please select at least one set of overlaid",
                 "predictions to which to apply weight polygon"))
    )

    #------------------------------------------------------
    ### Get input information
    overlaid.list <- strsplit(input$create_ens_reg_model, " ")
    overlaid.selected <- sapply(overlaid.list, function(i) as.numeric(i[[2]]))

    poly.filetype <- as.numeric(input$create_ens_reg_type)
    # poly.filetype.txt <- switch(poly.filetype, "CSV", "Raster", "SHP", "GDB")
    poly.filetype.txt <- switch(
      poly.filetype, "Excel .csv", "Shapefile", "Feature class"
    )


    #------------------------------------------------------
    ### Get/process weight polygon based on filetype
    if (poly.filetype == 1) {
      # .csv filetype
      poly.filename <- create_ens_reg_csv_process()[[2]]
      poly.sfc      <- create_ens_reg_csv_process()[[1]]
      weight.val <- ifelse(
        input$create_ens_reg_csv_weight_na,
        NA, input$create_ens_reg_csv_weight
      )

    # } else if (poly.filetype == 2) {
    #   # raster filetype
    #   poly.filename <- create_ens_reg_raster_read()[[2]]
    #   poly.sfc      <- create_ens_reg_raster_read()[[1]]
    #   weight.val <- ifelse(
    #     input$create_ens_reg_raster_weight_na,
    #     NA, input$create_ens_reg_raster_weight
    #   )

    } else if (poly.filetype == 2) {
      # .shp filetype
      poly.filename <- create_ens_reg_shp_read()[[2]]
      poly.sfc      <- create_ens_reg_shp_read()[[1]]
      weight.val <- ifelse(
        input$create_ens_reg_shp_weight_na,
        NA, input$create_ens_reg_shp_weight
      )

    } else { #poly.filetype == 3
      # .gdb filetype
      validate( #Best we can do with current .gdb loading system
        need(input$create_ens_reg_gdb_name,
             "Error: Please upload a file geodatabase feature class")
      )
      poly.filename <- create_ens_reg_gdb_read()[[2]]
      poly.sfc      <- create_ens_reg_gdb_read()[[1]]
      weight.val <- ifelse(
        input$create_ens_reg_gdb_weight_na,
        NA, input$create_ens_reg_gdb_weight
      )
    }

    validate(
      need(inherits(poly.sfc, "sfc"),
           paste("Error: There was an error loading the weight polygon;",
                 "please make sure the polygon is formatted correctly")) %then%
        need(length(poly.sfc) == 1,
             paste("Error: A weight polygon can only consist of one polygon;",
                   "please ensure that your file only has one polygon"))
    )

    poly.sf <- st_sf(Weight = weight.val, geometry = poly.sfc, agr = "constant")
    rm(poly.sfc)

    stopifnot(
      inherits(poly.sf, "sf"),
      is.numeric(overlaid.selected),
      nrow(poly.sf) == 1
    )


    #------------------------------------------------------
    ### Ensure that weight polygon overlaps with overlaid model(s)
    sapply(overlaid.selected, function(o.idx) {
      z <- suppressMessages(
        st_intersects(st_union(poly.sf), vals$overlaid.models[[o.idx]])
      )
      stopifnot(length(z) == 1)
      validate(
        need(length(z[[1]]) > 0,
             paste("Error: The provided weight polygon does not overlap",
                   "with overlaid model", o.idx))
      )
    })

    ### Ensure that new polygon doesn't overlap with loaded polygons
    ###  already assigned to same overlaid model
    sapply(overlaid.selected, function(o.idx) {
      if (!is.null(vals$ens.over.wpoly.sf[[o.idx]])) {
        mapply(function(poly.loaded, poly.idx) {
          x <- suppressMessages(st_intersection(poly.sf, poly.loaded))
          validate(
            need(nrow(x) == 0,
                 paste("Error: The GUI cannot load weight polygon because",
                       "polygon overlaps with weight polygon number",
                       poly.idx, "of overlaid model", o.idx))
          )
        },
        vals$ens.over.wpoly.sf[[o.idx]],
        seq_along(vals$ens.over.wpoly.sf[[o.idx]]))
      }
    })


    #------------------------------------------------------
    ### Add poly.sf and coverage val to applicable indices of vals$ens...
    ewf.selected     <- vals$ens.over.wpoly.filename[overlaid.selected]
    ewf.selected.new <- lapply(ewf.selected, function(l) c(l, poly.filename))
    vals$ens.over.wpoly.filename[overlaid.selected] <- ewf.selected.new

    ews.selected     <- vals$ens.over.wpoly.sf[overlaid.selected]
    ews.selected.new <- lapply(ews.selected, function(l) c(l, list(poly.sf)))
    vals$ens.over.wpoly.sf[overlaid.selected] <- ews.selected.new

    poly.coverage    <- input$create_ens_reg_coverage
    ewc.selected     <- vals$ens.over.wpoly.coverage[overlaid.selected]
    ewc.selected.new <- lapply(ewc.selected, function(l) c(l, poly.coverage))
    vals$ens.over.wpoly.coverage[overlaid.selected] <- ewc.selected.new


    #------------------------------------------------------
    ### Reset plot
    vals$ens.over.wpoly.plot <- NULL


    #------------------------------------------------------
    ### Output message
    paste(
      poly.filetype.txt, "weight polygon added as weight for:",
      paste(input$create_ens_reg_model, collapse = ", ")
    )
  }
)


###############################################################################
# File-specific loading and processing to sf object, and their respective flags

#----------------------------------------------------------
# CSV

### Flag for successfully loaded file
output$create_ens_reg_csv_flag <- reactive({
  isTruthy(create_ens_reg_csv_read())
})
outputOptions(output, "create_ens_reg_csv_flag",
              suspendWhenHidden = FALSE)

### Load and process
create_ens_reg_csv_read <- reactive({
  file.in <- input$create_ens_reg_csv_file
  validate(need(file.in, "Error: Please upload a .csv file "))

  # Ensure file extension is .csv (RStudio type, browser type)
  if (!(file.in$type %in% c("text/csv", "application/vnd.ms-excel"))) return()

  csv.data <- read.csv(file.in$datapath, stringsAsFactors = FALSE)

  return(list(file.in$name, csv.data))
})

create_ens_reg_csv_process <- reactive({
  withProgress(message = 'Processing csv polygon', value = 0.6, {
    csv.poly.list <- create_ens_reg_csv_read()
    csv.poly.filename <- csv.poly.list[[1]]
    csv.poly.data <- csv.poly.list[[2]]
    csv.poly.data[csv.poly.data == ""] <- NA

    if (min(csv.poly.data[, 1], na.rm = TRUE) > 180) {
      csv.poly.data[, 1] <- csv.poly.data[, 1] - 360
    }

    csv.poly.sfc <- pts_to_sfc_vertices_shiny(csv.poly.data[, 1:2], crs.ll, TRUE)
    # ^ Performs check_dateline()
    incProgress(0.3)

    # Transform weight polygon as necesary
    if (!identical(st_crs(csv.poly.sfc), vals$overlay.crs)) {
      csv.poly.sfc <- st_transform(csv.poly.sfc, vals$overlay.crs)
    }
    incProgress(0.1)
  })

  list(csv.poly.sfc, csv.poly.filename)
})


# #----------------------------------------------------------
# # GIS raster
#
# ### Flag for successfully loaded file
# output$create_ens_reg_raster_flag <- reactive({
#   isTruthy(create_ens_reg_raster_read())
# })
# outputOptions(output, "create_ens_reg_raster_flag",
#               suspendWhenHidden = FALSE)
#
#
# ### Load and process
# create_ens_reg_raster_read <- reactive({
#   file.in <- input$create_ens_reg_raster_file
#   validate(need(file.in, "Error: please upload a raster file"))
#
#   # Ensure file extension is .tif
#   if (!file.in$type %in% c("image/tiff", "")) return()
#
#
#   withProgress(message = "Loading GIS raster", value = 0.2, {
#     gis.file.raster <- try(
#       raster(file.in$datapath, band = 1), silent = TRUE
#     )
#     gis.file.success <- isTruthy(gis.file.raster)
#     incProgress(0.4)
#
#     # If specified file could be loaded as a raster, process raster
#     if (gis.file.success) {
#       gis.file.raster <- suppressMessages(st_union(
#         st_as_sfc(as(gis.file.raster, "SpatialPolygons"))
#       ))
#       stopifnot(inherits(gis.file.raster, "sfc"))
#       incProgress(0.1)
#
#       # Check that raster has valid crs
#       check_gis_crs(gis.file.raster)
#
#       # Adjust 0 - 360 data to -180 - 180 if needed
#       gis.file.raster <- check_dateline(gis.file.raster, 60)
#       incProgress(0.1)
#
#       # Transform weight polygon as necesary
#       if (!identical(st_crs(gis.file.raster), vals$overlay.crs)) {
#         gis.file.raster <- st_transform(gis.file.raster, vals$overlay.crs)
#       }
#
#       # Check that polygon(s) are valid
#       gis.file.raster <- check_valid(gis.file.raster, progress.detail = TRUE)
#       incProgress(0.1)
#     }
#   })
#
#   if(!gis.file.success) {
#     NULL
#   } else {
#     list(gis.file.raster, file.in$name)
#   }
# })


#----------------------------------------------------------
# GIS shp

### Flag for successfully loaded file
output$create_ens_reg_shp_flag <- reactive({
  isTruthy(create_ens_reg_shp_read())
})
outputOptions(
  output, "create_ens_reg_shp_flag", suspendWhenHidden = FALSE
)

### Load and process
create_ens_reg_shp_read <- reactive({
  files.in <- input$create_ens_reg_shp_files
  validate(need(files.in, "Error: Please upload the files of a shapefile"))

  withProgress(message = "Processing GIS shapefile", value = 0.3, {
    gis.file.shp <- read.shp.shiny(files.in)
    incProgress(0.4)

    gis.file.success <- isTruthy(gis.file.shp)

    if (gis.file.success) {
      gis.file.shp <- suppressMessages(st_union(gis.file.shp))
      incProgress(0.1)

      # Adjust 0 - 360 data to -180 - 180 if needed
      gis.file.shp <- check_dateline(gis.file.shp, 60)
      incProgress(0.1)

      # Transform weight polygon as necesary
      if (!identical(st_crs(gis.file.shp), vals$overlay.crs)) {
        gis.file.shp <- st_transform(gis.file.shp, vals$overlay.crs)
      }

      # Check that polygon(s) are valid
      gis.file.shp <- check_valid(gis.file.shp, progress.detail = TRUE)
      incProgress(0.1)
    }
  })

  if (!gis.file.success) {
    NULL
  } else {
    list(gis.file.shp, strsplit(files.in$name[1], "[.]")[[1]][1])
  }
})


#----------------------------------------------------------
# GIS gdb

### Flag for successfully loaded file
output$create_ens_reg_gdb_flag <- reactive({
  isTruthy(create_ens_reg_gdb_read())
})
outputOptions(output, "create_ens_reg_gdb_flag", suspendWhenHidden = FALSE)


### Load and process
create_ens_reg_gdb_read <- eventReactive(
  input$create_ens_reg_gdb_load,
  {
    gdb.path <- input$create_ens_reg_gdb_path
    gdb.name <- input$create_ens_reg_gdb_name

    withProgress(message = "Processing GIS .gdb file", value = 0.3, {
      gis.file.gdb <- try(st_read(gdb.path, gdb.name, quiet = TRUE),
                          silent = TRUE)
      incProgress(0.4)

      gis.file.success <- isTruthy(gis.file.gdb)
      if (gis.file.success) {
        gis.file.gdb <- suppressMessages(st_union(gis.file.gdb))
        incProgress(0.1)

        # Adjust 0 - 360 data to -180 - 180 if needed
        gis.file.gdb <- check_dateline(gis.file.gdb, 60)
        incProgress(0.1)

        # Transform weight polygon as necesary
        if (!identical(st_crs(gis.file.gdb), vals$overlay.crs)) {
          gis.file.gdb <- st_transform(gis.file.gdb, vals$overlay.crs)
        }

        # Check that polygon(s) are valid
        gis.file.gdb <- check_valid(gis.file.gdb, progress.detail = TRUE)
        incProgress(0.1)
      }
    })

    if (!gis.file.success) {
      NULL
    } else {
      list(gis.file.gdb, gdb.name)
    }
  })


###############################################################################
