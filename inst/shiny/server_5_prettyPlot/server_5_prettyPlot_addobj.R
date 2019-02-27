###############################################################################
# Additional objects, adding and removing

#------------------------------------------------------------------------------
#pretty_addobj_list() is in sever_5_prettyPlot_prep.R

#------------------------------------------------------------------------------
# Remove loaded objects when section is closed
observeEvent(input$pretty_addobj, {
  if (!input$pretty_addobj) vals$pretty.addobj <- NULL
})


#------------------------------------------------------------------------------
### Add additional poly information to reactive values
pretty_addobj_add <- eventReactive(input$pretty_addobj_add_execute, {
  #--------------------------------------------------------
  valid.message <- ifelse(
    input$pretty_addobj_type == 1,
    "Error: The point size entry must be a number",
    "Error: The line width of polygon borders entry must be a number"
  )
  validate(
    need(input$pretty_addobj_cexlwd, valid.message)
  )

  # Prep
  #------------------------------------
  withProgress(message = "Processing additional polygon", value = 0.3, {
    if (input$pretty_addobj_which == 4) {
      if (input$pretty_addobj_own_type == 1) {
        validate(
          need(input$pretty_addobj_own_csv_file,
               "Error: Please upload a .csv file")
        )
        addobj.obj      <- pretty_addobj_own_csv_process()
        addobj.obj.text <- pretty_addobj_own_csv_read()[[2]]
        addobj.obj.own  <- 1

      } else if (input$pretty_addobj_own_type == 2) {
        validate(
          need(input$input$pretty_addobj_own_shp_files,
               "Error: Please upload shapefile files")
        )
        addobj.obj      <- req(pretty_addobj_own_shp_process())
        addobj.obj.text <- req(pretty_addobj_own_shp_read())[[2]]
        addobj.obj.own  <- 2

      } else { #input$pretty_addobj_own_type == 3
        validate(
          need(isTruthy(input$input$pretty_addobj_own_gdb_load) &&
                 isTruthy(input$pretty_addobj_own_gdb_name),
               "Error: Please upload a feature class")
        )
        addobj.obj      <- req(pretty_addobj_own_gdb_process())
        addobj.obj.text <- req(pretty_addobj_own_gdb_read())[[2]]
        addobj.obj.own  <- 3
      }

    } else {
      addobj.obj <- switch(
        as.numeric(input$pretty_addobj_which),
        vals$overlay.bound, vals$overlay.land, vals$eval.data, NULL
      )
      addobj.obj.text <- switch(
        as.numeric(input$pretty_addobj_which),
        "Study area polygon", "Erasing polygon", "Validation data points", "WRONG"
      )
      addobj.obj.own  <- 1
    }
    incProgress(0.5)

    #------------------------------------
    addobj.col.ptfill <- ifelse(
      input$pretty_addobj_color_ptfillcheck,
      NA, input$pretty_addobj_color_ptfill
    )

    addobj.col.absborder <- ifelse (
      input$pretty_addobj_color_absbordercheck |
        (input$pretty_addobj_type == 1 && input$pretty_addobj_which != 3),
      NA, input$pretty_addobj_color_absborder
    )
    incProgress(0.2)


    #--------------------------------------------------------
    # Add to reactive val list
    vals$pretty.addobj <- c(
      vals$pretty.addobj,
      list(list(
        obj = addobj.obj,
        obj.text = addobj.obj.text,
        obj.own = addobj.obj.own, #only for update; file type (.csv .shp, .gdb)
        obj.which = input$pretty_addobj_which, #only for update; which addobj
        obj.type = input$pretty_addobj_type,
        obj.order = input$pretty_addobj_order,
        col.ptfill = addobj.col.ptfill,
        col.absborder = addobj.col.absborder,
        pchlty = as.numeric(input$pretty_addobj_pchlty),
        cexlwd = input$pretty_addobj_cexlwd
      ))
    )
  })

  ""
})


#------------------------------------------------------------------------------
### Remove loaded additional polygon
pretty_addobj_remove <- eventReactive(input$pretty_addobj_remove_execute, {
  x <- input$pretty_addobj_table_out_rows_selected

  validate(
    need(x, "Error: You must select a row from the 'Loaded additional objects' table")
  )

  vals$pretty.addobj <- vals$pretty.addobj[-x]
  if (length(vals$pretty.addobj) == 0) vals$pretty.addobj <- NULL

  ""
})


#------------------------------------------------------------------------------
### Display table for 'added' additional polygons
pretty_addobj_table <- reactive({
  validate(
    need(vals$pretty.addobj, "No additional objects have been loaded"),
    errorClass = "validation2"
  )

  x <- data.frame(do.call(
    rbind, lapply(vals$pretty.addobj, function(i) {
      c(i$obj.text, ifelse(i$obj.type == 1, "Point", "Polygon"),
        ifelse(i$obj.order == 1, "Behind SDM", "In front of SDM"))
    })
  ), stringsAsFactors = FALSE)

  purrr::set_names(x, c("Object", "Object type", "Draw order"))
})


###############################################################################
# User file-specific loading and processing to sf object, and their respective flags

#------------------------------------------------------------------------------
# .csv

### Flag for successfully loaded file
output$pretty_addobj_own_csv_flag <- reactive({
  isTruthy(pretty_addobj_own_csv_read())
})
outputOptions(output, "pretty_addobj_own_csv_flag",
              suspendWhenHidden = FALSE)

### Load and process
pretty_addobj_own_csv_read <- reactive({
  file.in <- input$pretty_addobj_own_csv_file
  req(file.in)

  # Ensure file extension is .csv (RStudio type, browser type)
  if (!(file.in$type %in% c("text/csv", "application/vnd.ms-excel"))) return()

  csv.data <- read.csv(file.in$datapath, stringsAsFactors = FALSE)

  return(list(csv.data, file.in$name))
})

pretty_addobj_own_csv_process <- reactive({
  csv.data     <- pretty_addobj_own_csv_read()[[1]]
  csv.data[csv.data == ""] <- NA
  names(csv.data) <- c("lon", "lat")

  if (min(csv.data$lon, na.rm = TRUE) > 180) {
    csv.data$lon <- csv.data$lon - 360
  }

  if (input$pretty_addobj_type == 1) {
    # Points
    validate(
      need(!any(is.na(csv.data)),
           paste("Error: For points, no entries in the longitude or",
                 "latitude columns can be blank or 'NA'")
      )
    )

    csv.sfc <- try(
      st_geometry(st_as_sf(csv.data, coords = c("lon","lat"), crs = crs.ll)),
      silent = TRUE
    )

    csv.sfc <- try(check_dateline(csv.sfc, 10, FALSE), silent = TRUE)
    # Don't need to check validity for points

  } else {
    # Polygon(s)
    csv.sfc <- pts2poly_vertices_shiny(csv.data[, 1:2], crs.ll, FALSE)
    # ^ checks dateline and validity
  }

  validate(
    need(csv.sfc,
         paste("Error: The GUI was unable to process the provided .csv file.",
               "Please ensure that the .csv file has the longitude points",
               "in the first column, the latitude points in the second",
               "column, and that the entries are valid"))
  )

  csv.sfc
})


#------------------------------------------------------------------------------
# GIS shp

### Flag for successfully loaded file
output$pretty_addobj_own_shp_flag <- reactive({
  isTruthy(pretty_addobj_own_shp_read())
})
outputOptions(
  output, "pretty_addobj_own_shp_flag", suspendWhenHidden = FALSE
)

### Read/upload
pretty_addobj_own_shp_read <- reactive({
  files.in <- req(input$pretty_addobj_own_shp_files)

  withProgress(message = "Uploading object", value = 0.6, {
    gis.file.shp <- read.shp.shiny(files.in)
    incProgress(0.4)
    shp.name <- strsplit(files.in$name[1], "[.]")[[1]][1]
  })

  if (isTruthy(gis.file.shp)) list(gis.file.shp, shp.name) else NULL
})

### Process
pretty_addobj_own_shp_process <- reactive({
  addobj_gis_check_shiny(
    pretty_addobj_own_shp_read()[[1]], input$pretty_addobj_type
  )
})


#------------------------------------------------------------------------------
# GIS gdb

### Flag for successfully loaded file
output$pretty_addobj_own_gdb_flag <- reactive({
  isTruthy(pretty_addobj_own_gdb_read())
})
outputOptions(output, "pretty_addobj_own_gdb_flag", suspendWhenHidden = FALSE)


### Read/upload
pretty_addobj_own_gdb_read <- eventReactive(
  input$pretty_addobj_own_gdb_load,
  {
    gdb.path <- input$pretty_addobj_own_gdb_path
    gdb.name <- input$pretty_addobj_own_gdb_name

    withProgress(message = "Uploading object", value = 0.6, {
      gis.file.gdb <- try(st_read(gdb.path, gdb.name, quiet = TRUE),
                          silent = TRUE)
      incProgress(0.6)
    })

    if (isTruthy(gis.file.gdb)) list(gis.file.gdb, gdb.name) else NULL
  })

### Process
pretty_addobj_own_gdb_process <- reactive({
  addobj_gis_check_shiny(
    pretty_addobj_own_gdb_read()[[1]], input$pretty_addobj_type
  )
})


#------------------------------------------------------------------------------
# Used by shp and gdb functions
addobj_gis_check_shiny <- function(gis.file, obj.type) {
  gis.sfc <- st_geometry(gis.file)
  gis.file <- suppressMessages(st_union(gis.file))
  gis.sfc <- check_dateline(gis.sfc, 60, FALSE)

  validate.message <- paste(
    "Error: The geometry of object in the provided GIS file does not match",
    "the selected object type.",
    "Please ensure that you selected the correct object type",
    "(point or polygon) and that the provided GIS file",
    "contains either all points or all polygons"
  )

  if (obj.type == 1) {
    if (!(any(grepl("POINT", class(gis.sfc))))) validate(validate.message)

  } else {
    if (!(any(grepl("POLYGON", class(gis.sfc))))) validate(validate.message)
  }

  gis.sfc
}

###############################################################################
