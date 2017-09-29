### Code for creating weighted ensembles using method 4: weights from polygons


###############################################################################
### Create weighted ensemble using polygon weights
create_ens_weighted_poly <- reactive({
  validate(
    need(sum(!sapply(vals$ens.over.wpoly.filename, is.null)) > 0, 
         paste("Error: Please load at least one weight polygon to", 
               "use this weighted ensembling method"))
  )
  
  overlaid.data <- create_ens_weights_poly_preds() # Already weighted
  base.sp <- vals$overlay.base.sp
  
  data.ens <- data.frame(Pred.ens = apply(overlaid.data, 1, mean, na.rm = TRUE))
  
  SpatialPolygonsDataFrame(base.sp, data.ens, match.ID = FALSE)
})

### Get weights based on loaded polygons
create_ens_weights_poly_preds <- reactive({
  # ###########################################################
  # # Testing code
  # browser()
  # i <- 2; j <- 1
  # pred.spdf <- vals$overlaid.models[[i]]
  # # wpoly.filename <- vals$ens.over.wpoly.filename[[i]]
  # wpoly.spdf.list <- vals$ens.over.wpoly.spdf[[i]]
  # wpoly.coverage.vec <- vals$ens.over.wpoly.coverage[[i]]
  # wpoly.spdf <- wpoly.spdf.list[[j]]
  # ###########################################################
  
  code.out <- mapply(function(pred.spdf, wpoly.spdf.list, wpoly.coverage.vec) {
    if (is.null(wpoly.spdf.list)) {
      pred.spdf$Pred.overlaid
    } else {
      pred.w.list <- mapply(function(wpoly.spdf, wpoly.coverage) {
        if (length(unique(wpoly.spdf$Weight)) == 1) {
          poly.weight(pred.spdf, wpoly.spdf, wpoly.coverage)
        } else {
          validate(
            need(FALSE, 
                 "Error: Can't handle multiple weights within one wpoly (yet)")
          )
          
          # wpoly.vals <- unique(wpoly.spdf$Weight)
          # wpoly.sp.all <- gUnaryUnion(wpoly.spdf)
          # wpoly.val.list <- lapply(wpoly.vals, function(val) {
          #   temp <- wpoly.spdf[wpoly.spdf$Weight == val,]
          #   temp.sp <- gUnaryUnion(temp)
          #   temp.df <- data.frame(Weight = val)
          #   SpatialPolygonsDataFrame(temp.sp, temp.df, match.ID = FALSE)
          # })
          # 
          # pred.wpoly.over <- over(pred.spdf, wpoly.sp.all, 
          #                             returnList = TRUE)
          # over.which <- which(sapply(pred.wpoly.over, length) > 0)
          # pred.wpoly.int <- intersect(pred.spdf, wpoly.sp.all)
          # 
          # validate(
          #   need(identical(pred.wpoly.int$Pred.overlaid, 
          #                  pred.spdf$Pred.overlaid[over.which]), 
          #        "Error: Weighting method 4: Indices don't match ")
          # )
          # 
          # a.ratio <- area(pred.wpoly.int) / area(pred.spdf)[over.which]
          # idx.toweight <- over.which[a.ratio >= (wpoly.coverage / 100)]
          # 
          # 
          # ab <- do.call(bind, wpoly.val.list)
          # x <- which(!is.na(over(pred.spdf, wpoly.val.list[[1]])))
          # y <- which(!is.na(over(pred.spdf, wpoly.val.list[[2]])))
          # z <- x[x %in% y]
          # z.spdf <- pred.spdf[z,]
          
          # res.df <- lapply(z, function(i) {
          #   curr.idx <- over.base.idx[[i]]
          #   if(length(curr.idx) != 0) {
          #     temp <- intersect(pol.base[i,], pol.spdf[curr.idx,])
          #     if(!is.null(temp)) temp
          #     else NA
          #   }
          #   else NA
          # })
        }
        
      }, 
      wpoly.spdf.list, wpoly.coverage.vec, SIMPLIFY = FALSE)
      
      pred.out <- pred.spdf$Pred.overlaid
      for(i in pred.w.list) {
        i.idx <- i[[2]]
        i.vals <- i[[1]][i.idx]
        pred.out[i.idx] <- i.vals
      }
      
      pred.out
    }
  }, 
  vals$overlaid.models, vals$ens.over.wpoly.spdf, 
  vals$ens.over.wpoly.coverage, 
  SIMPLIFY = FALSE)
  
  as.data.frame(code.out)
})


### Function for weighting overlapping areas by weight in weight poly
# Returns list of new weighted predictions and inidices that were weighted
poly.weight <- function(poly.pred, poly.w, coverage) {
  poly.w.sp <- gUnaryUnion(poly.w)
  poly.w.df <- data.frame(Weight = poly.w$Weight[1])
  poly.w <- SpatialPolygonsDataFrame(poly.w.sp, poly.w.df,
                                     match.ID = FALSE)
  
  overlaid.wpoly.over <- over(poly.pred, poly.w,
                              returnList = TRUE)
  over.which <- which(sapply(overlaid.wpoly.over, nrow) > 0)
  overlaid.wpoly.int <- intersect(poly.pred, poly.w)
  
  validate(
    need(identical(overlaid.wpoly.int$Pred.overlaid,
                   poly.pred$Pred.overlaid[over.which]),
         "Error: Weighting method 4: Indices don't match ")
  )
  
  a.ratio <- area(overlaid.wpoly.int) / area(poly.pred)[over.which]
  idx.toweight <- over.which[a.ratio >= (coverage / 100)]
  
  pred.out <- poly.pred$Pred.overlaid
  pred.out[idx.toweight] <- pred.out[idx.toweight] * poly.w$Weight
  
  list(pred.out, idx.toweight)
}
######################################################################
### Code for ^ not in function form
# wpoly.sp <- gUnaryUnion(wpoly.spdf)
# wpoly.df <- data.frame(Weight = wpoly.spdf$Weight)
# wpoly.spdf <- SpatialPolygonsDataFrame(wpoly.sp, wpoly.df, 
#                                    match.ID = FALSE)
# 
# pred.wpoly.over <- over(pred.spdf, wpoly.spdf, 
#                             returnList = TRUE)
# over.which <- which(sapply(pred.wpoly.over, nrow) > 0)
# pred.wpoly.int <- intersect(pred.spdf, wpoly.spdf)
# 
# validate(
#   need(identical(pred.wpoly.int$Pred.overlaid, 
#                  pred.spdf$Pred.overlaid[over.which]), 
#        "Error: Weighting method 4: Indices don't match ")
# )
# 
# a.ratio <- area(pred.wpoly.int) / area(pred.spdf)[over.which]
# idx.toweight <- over.which[a.ratio >= (wpoly.coverage / 100)]
# 
# pred.out <- pred.spdf$Pred.overlaid
# pred.out[idx.toweight] <- pred.out[idx.toweight] * wpoly.spdf$Weight
# 
# list(pred.out, idx.toweight)
######################################################################


###############################################################################
### Plot preview of weight polygons
create_ens_weights_poly_preview <- eventReactive(
  input$create_ens_weights_poly_preview_execute, 
  {
    overlaid.which <- as.numeric(input$create_ens_weights_poly_preview_model)
    plot(vals$overlaid.models[[overlaid.which]], axes = T, 
         col = "black", border = NA)
    
    if (!is.null(vals$ens.over.wpoly.spdf[[overlaid.which]])) {
      for(spdf.toplot in vals$ens.over.wpoly.spdf[[overlaid.which]]) {
        plot(spdf.toplot, add = T, col = NA, border = "red")
      }
    }
  }
)


###############################################################################
### Remove loaded weight polygons
create_ens_weights_poly_remove <- eventReactive(
  input$create_ens_weights_poly_remove_execute, 
  {
    validate(
      need(!is.null(input$create_ens_weights_poly_remove_choices), 
           "Error: Please select at least one weighted polygon to remove")
    )
    
    # Get indices of wpoly objects to remove
    poly.toremove.idx <- input$create_ens_weights_poly_remove_choices
    poly.toremove.idx <- lapply(strsplit(poly.toremove.idx, ", "), as.numeric)
    poly.toremove.df <- data.frame(t(data.frame(poly.toremove.idx)))
    
    poly.toremove.df.list <- by(poly.toremove.df, poly.toremove.df[,1], 
                                function(j) c(j[,2]))
    
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
      
      if (!is.null(idx.poly)){
        x <- vals$ens.over.wpoly.filename[[idx.model]][-idx.poly]
        y <- vals$ens.over.wpoly.spdf[[idx.model]][-idx.poly]
        z <- vals$ens.over.wpoly.coverage[[idx.model]][-idx.poly]
        
        if (length(x) == 0) {
          vals$ens.over.wpoly.filename[idx.model] <- list(NULL)
          vals$ens.over.wpoly.spdf[idx.model] <- list(NULL)
          vals$ens.over.wpoly.coverage[idx.model] <- list(NULL)
        } else {
          vals$ens.over.wpoly.filename[[idx.model]] <- x
          vals$ens.over.wpoly.spdf[[idx.model]] <- y
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
  if(input$create_ens_table_subset) {
    ens.selected <- input$create_ens_datatable_rows_selected
    models.which <- models.which[models.which %in% ens.selected]
  }
  
  req(length(models.which) >= 2)
  
  # browser()
  overlaid.names <- paste("Overlaid", models.which)
  if (all(sapply(vals$ens.over.wpoly.filename[models.which], is.null))) {
    overlaid.filenames <- ""
    overlaid.weights <- ""
    overlaid.coverage <- ""
  } else {
    overlaid.filenames <- sapply(vals$ens.over.wpoly.filename, 
                                 paste, collapse = ", ")[models.which]
    
    overlaid.weights <- sapply(vals$ens.over.wpoly.spdf, function(l.spdf) {
      paste(lapply(l.spdf, function(spdf) {
        ifelse(length(unique(spdf$Weight)) > 1, "Multiple", spdf$Weight[1])
      }), 
      collapse = ", ")
    })[models.which]
    
    overlaid.coverage <- sapply(vals$ens.over.wpoly.coverage, 
                                paste, collapse = ", ")[models.which]
  }
  
  table.out <- data.frame(overlaid.names, overlaid.filenames, 
                          overlaid.weights, overlaid.coverage, 
                          stringsAsFactors = FALSE)
  names(table.out) <- c("Predictions", "File(s)", "Weight(s)", "Coverage(s)")
  
  table.out
})


###############################################################################
### Add filename, weighted polygon, and coverage percentage to...
# ...vals$ens.over.wpoly... objects
create_ens_weights_poly_add <- eventReactive(
  input$create_ens_weights_poly_add_execute, 
  {
    validate(
      need(!is.null(input$create_ens_weights_poly_model), 
           "Error: Please select at least one overlaid model")
    )
    
    overlaid.list <- strsplit(input$create_ens_weights_poly_model, " ")
    overlaid.selected <- sapply(overlaid.list, function(i) as.numeric(i[[2]]))
    
    poly.filetype <- as.numeric(input$create_ens_weights_poly_type)
    poly.filetype.txt <- switch(poly.filetype, "CSV", "Raster", "SHP", "GDB")
    
    
    ### Get/process weight polygon based on filetype
    # .csv
    if (poly.filetype == 1) {
      poly.sp <- create_ens_weights_poly_csv_process()[[1]]
      weight.df <- data.frame(Weight = input$create_ens_weights_poly_csv_weight)
      
      validate(
        need(length(poly.sp) == 1, 
             "Error: .csv poly length does not equal 1")
      )
      
      poly.filename <- create_ens_weights_poly_csv_process()[[2]]
      poly.spdf <- SpatialPolygonsDataFrame(poly.sp, weight.df, 
                                            match.ID = FALSE)
    }
    # .tif
    else if (poly.filetype == 2) {
      poly.list <- create_ens_weights_poly_raster_read()
      poly.filename <- poly.list[[2]]
      poly.spdf <- poly.list[[1]]
    } 
    # .shp
    else if (poly.filetype == 3) { #shp
      poly.list <- create_ens_weights_poly_shp_read()
      poly.filename <- poly.list[[2]]
      poly.spdf <- poly.list[[1]]
      
      if (input$create_ens_weights_poly_shp_weight_type == 1) {
        weight.val <- input$create_ens_weights_poly_shp_weight
        poly.spdf@data <- data.frame(Weight = rep(weight.val, 
                                                  length(poly.spdf)))
      } else {
        poly.spdf <- poly.spdf[input$create_ens_weights_poly_shp_field]
        names(poly.spdf) <- "Weight"
      }
    }
    # .gdb
    else if (poly.filetype == 4) {
      poly.list <- create_ens_weights_poly_gdb_read()
      poly.filename <- poly.list[[2]]
      poly.spdf <- poly.list[[1]]
      
      if (input$create_ens_weights_poly_gdb_weight_type == 1) {
        weight.val <- input$create_ens_weights_poly_gdb_weight
        poly.spdf@data <- data.frame(Weight = rep(weight.val, 
                                                  length(poly.spdf)))
      } else {
        poly.spdf <- poly.spdf[input$create_ens_weights_poly_gdb_field]
        names(poly.spdf) <- "Weight"
      }
    } 
    else {
      validate(
        need(FALSE, 
             "Error: create_ens_weights_poly_add() filetype error")
      )
    }
    
    
    ### Ensure that weight polygon has same crs as overlaid models
    if (!identical(crs(poly.spdf), vals$overlay.crs)) {
      poly.spdf <- spTransform(poly.spdf, vals$overlay.crs)
    }
    
    ### Make sure that new polygon doesn't overlap with loaded polygons
    # assigned to same overlaid model
    sapply(overlaid.selected, function(overlaid.idx) {
      if (!is.null(vals$ens.over.wpoly.spdf[[overlaid.idx]])) {
        mapply(function(poly.loaded, poly.idx) {
          validate(
            need(is.null(gIntersection(poly.spdf, poly.loaded, byid = TRUE)), 
                 paste("Error: Cannot load weight polygon because", 
                       "polygon overlaps with weight polygon number", 
                       poly.idx, "of overlaid model", overlaid.idx))
          )
        }, 
        vals$ens.over.wpoly.spdf[[overlaid.idx]], 
        seq_along(vals$ens.over.wpoly.spdf[[overlaid.idx]]))
      }
    })
    
    ### Add poly.spdf and coverage val to applicable indices of vals$ens...
    ewf.selected <- vals$ens.over.wpoly.filename[overlaid.selected]
    ewf.selected.new <- lapply(ewf.selected, function(l) c(l, poly.filename))
    vals$ens.over.wpoly.filename[overlaid.selected] <- ewf.selected.new
    
    ews.selected <- vals$ens.over.wpoly.spdf[overlaid.selected]
    ews.selected.new <- lapply(ews.selected, function(l) c(l, poly.spdf))
    vals$ens.over.wpoly.spdf[overlaid.selected] <- ews.selected.new
    
    poly.coverage <- input$create_ens_weights_poly_coverage
    ewc.selected <- vals$ens.over.wpoly.coverage[overlaid.selected]
    ewc.selected.new <- lapply(ewc.selected, function(l) c(l, poly.coverage))
    vals$ens.over.wpoly.coverage[overlaid.selected] <- ewc.selected.new
    
    ### Output message
    paste(poly.filetype.txt, "weight polygon added as weight for:", 
          paste(input$create_ens_weights_poly_model, collapse = ", "))
  }
)


###############################################################################
# File-specific loading and processing, and their respective flags

###########################################################
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
  withProgress(message = 'Loading csv polygon', value = 0.7, {
    csv.poly.list <- create_ens_weights_poly_csv_read()
    csv.poly.filename <- csv.poly.list[[1]]
    csv.poly.data <- csv.poly.list[[2]]
    csv.poly.data[csv.poly.data == ""] <- NA
    
    # Split polygons by rows signified by NAs
    list.data <- unname(by(csv.poly.data, cumsum(is.na(csv.poly.data[,1])), 
                           function(i) {
                             data.frame(na.omit(i))
                           }))
    
    # Create list of Polygon objects
    list.polygon <- lapply(list.data, function(j) {
      Polygon(cbind(j[,1], j[,2]), hole = FALSE)
    })
    
    # Create SpatialPolygons object (DF with weight added later)
    csv.poly.sp <- SpatialPolygons(list(Polygons(list.polygon, 
                                                 ID = "poly_weight")))
    crs(csv.poly.sp) <- crs.ll
    
    if (!suppressWarnings(gIsValid(csv.poly.sp))) {
      warning("Loaded csv poly isn't valid, likely due to self-intersection")
    }
    incProgress(0.3)
  })
  
  list(csv.poly.sp, csv.poly.filename)
})


###########################################################
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
  
  
  withProgress(message = "Loading GIS raster", value = 0.3, {
    weight.type <- input$create_ens_weights_poly_raster_weight_type
    raster.band.num <- ifelse(weight.type == 1, 1, 
                              input$create_ens_weights_poly_raster_band)
    gis.file.raster <- try(raster(file.in$datapath, band = raster.band.num), 
                           silent = TRUE)
    gis.file.success <- isTruthy(gis.file.raster)
    incProgress(0.4)
    
    # If specified file could be loaded as a raster, process raster
    if (gis.file.success) {
      poly.pix <- as(gis.file.raster, "SpatialPixelsDataFrame")
      names(poly.pix) <- "Weight"
      
      if (weight.type == 1) {
        poly.pix$Weight <- input$create_ens_weights_poly_raster_weight
      }
      poly.spdf <- as(poly.pix, "SpatialPolygonsDataFrame")
      
      # if (!identical(crs(poly.spdf), crs.ll)) {
      #   poly.spdf <- spTransform(poly.spdf, crs.ll)
      # }
      
      # Run dateline correction function here..?
      ext <- extent(poly.spdf)
      validate(
        need(all(ext@xmax <= 180 & ext@xmin >= -180),
             "Error: Raster extent is not -180 to 180 degrees")
      )
      incProgress(0.1)
    }
  })
  
  if(!gis.file.success) {
    NULL
  } else {
    list(poly.spdf, file.in$name)
  }
})


###########################################################
# GIS shp

### Flag for successfully loaded file
output$create_ens_weights_poly_shp_flag <- reactive({
  !is.null(create_ens_weights_poly_shp_read())
})
outputOptions(output, "create_ens_weights_poly_shp_flag", 
              suspendWhenHidden = FALSE)

### Load and process
create_ens_weights_poly_shp_read <- reactive({
  files.in <- input$create_ens_weights_poly_shp_files
  req(files.in)
  
  withProgress(message = "Loading GIS shapefile", value = 0.3, {
    gis.file.shp <- read.shp.in(files.in)
    incProgress(0.5)
    
    gis.file.success <- isTruthy(gis.file.shp)
    if (gis.file.success) {
      # if (!identical(crs(gis.file.shp), crs.ll)) {
      #   gis.file.shp <- spTransform(gis.file.shp, crs.ll)
      # }
      
      # Run dateline correction function here..?
      ext <- extent(gis.file.shp)
      validate(
        need(all(ext@xmax <= 180 & ext@xmin >= -180),
             "Error: Raster extent is not -180 to 180 degrees")
      )
      incProgress(0.1)
      
      print(class(gis.file.shp)[1])
      if (class(gis.file.shp)[1] == "SpatialPolygons") {
        gis.file.shp.df <- data.frame(Weight = rep(NA, length(gis.file.shp)))
        gis.file.shp <- SpatialPolygonsDataFrame(gis.file.shp, 
                                                 gis.file.shp.df, 
                                                 match.ID = FALSE)
      }
      incProgress(0.1)
    }
  })
  
  if(!gis.file.success) {
    NULL
  } else {
    list(gis.file.shp, strsplit(files.in$name[1], "[.]")[[1]][1])
  }
})


###########################################################
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
      gis.file.gdb <- try(readOGR(gdb.path, gdb.name, verbose = FALSE), 
                          silent = TRUE)
      incProgress(0.5)
      
      gis.file.success <- isTruthy(gis.file.gdb)
      if (gis.file.success) {
        # if (!identical(crs(gis.file.gdb), crs.ll)) {
        #   gis.file.gdb <- spTransform(gis.file.gdb, crs.ll)
        # }
        
        # Run dateline correction function here..?
        ext <- extent(gis.file.gdb)
        validate(
          need(all(ext@xmax <= 180 & ext@xmin >= -180),
               "Error: Raster extent is not -180 to 180 degrees")
        )
        incProgress(0.1)
        
        if (class(gis.file.gdb)[1] == "SpatialPolygons") {
          gis.file.gdb.df <- data.frame(Weight = rep(NA, length(gis.file.gdb)))
          gis.file.gdb <- SpatialPolygonsDataFrame(gis.file.gdb, 
                                                   gis.file.gdb.df, 
                                                   match.ID = FALSE)
        }
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