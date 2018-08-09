###############################################################################
# Functions; renderUI's below

#------------------------------------------------------------------------------
#pretty_plot_addobj_list() is in sever_5_prettyPlot_prep.R

#------------------------------------------------------------------------------
observeEvent(input$pretty_plot_addobj, {
  if (!input$pretty_plot_addobj) vals$pretty.addobj <- NULL
})


#------------------------------------------------------------------------------
### Add additional poly information to reactive values
pretty_plot_addobj_add <- eventReactive(input$pretty_plot_addobj_add_execute, {
  #--------------------------------------------------------
  # Prep
  #------------------------------------
  if (input$pretty_plot_addobj_which == 4) {
    if (input$pretty_plot_addobj_own_type == 1) {
      addobj.obj      <- pretty_plot_addobj_own_csv_process()
      addobj.obj.text <- pretty_plot_addobj_own_csv_read()[[2]]

    } else if (input$pretty_plot_addobj_own_type == 2) {
      addobj.obj      <- req(pretty_plot_addobj_own_shp_process())
      addobj.obj.text <- req(pretty_plot_addobj_own_shp_read())[[2]]

    } else { #input$pretty_plot_addobj_own_type == 3
      addobj.obj      <- req(pretty_plot_addobj_own_gdb_process())
      addobj.obj.text <- req(pretty_plot_addobj_own_gdb_read())[[2]]
    }

  } else {
    addobj.obj <- switch(
      as.numeric(input$pretty_plot_addobj_which),
      vals$overlay.bound, vals$overlay.land, vals$eval.data, NULL
    )
    addobj.obj.text <- switch(
      as.numeric(input$pretty_plot_addobj_which),
      "Study area polygon", "Land polygon", "Validation data points", "WRONG"
    )
  }

  #------------------------------------
  if (input$pretty_plot_addobj_color_ptfillcheck) {
    addobj.col.ptfill <- NA
  } else {
    addobj.col.ptfill <- input$pretty_plot_addobj_color_ptfill
  }

  #------------------------------------
  if (input$pretty_plot_addobj_color_absbordercheck) {
    addobj.col.absborder <- NA
  } else {
    addobj.col.absborder <- input$pretty_plot_addobj_color_absborder
  }


  #--------------------------------------------------------
  # Add to reactive val list
  vals$pretty.addobj <- c(
    vals$pretty.addobj,
    list(list(
      obj = addobj.obj,
      obj.text = addobj.obj.text,
      obj.type =input$pretty_plot_addobj_type,
      pre.sdm = input$pretty_plot_addobj_order == 1,
      col.ptfill = addobj.col.ptfill,
      col.absborder = addobj.col.absborder,
      pchlty = as.numeric(input$pretty_plot_addobj_pchlty),
      cexlwd = input$pretty_plot_addobj_cexlwd
    ))
  )

  ""
})


#------------------------------------------------------------------------------
### Remove loaded additional polygon
pretty_plot_addobj_remove <- eventReactive(input$pretty_plot_addobj_remove_execute, {
  x <- input$pretty_plot_addobj_table_out_rows_selected

  validate(
    need(x, "Error: A loaded additional object must be selected to remove")
  )

  vals$pretty.addobj <- vals$pretty.addobj[-x]
  if (length(vals$pretty.addobj) == 0) vals$pretty.addobj <- NULL

  ""
})


#------------------------------------------------------------------------------
### Display table for 'added' additional polygons
pretty_plot_addobj_table <- reactive({
  validate(
    need(vals$pretty.addobj, "No additional objects have been loaded"),
    errorClass = "validation2"
  )

  # TODO: add more data columns in table?
  x <- data.frame(do.call(
    rbind, lapply(vals$pretty.addobj, function(i) {
      c(i$obj.text, ifelse(i$obj.type == 1, "Point", "Polygon"), i$pre.sdm)
    })
  ), stringsAsFactors = FALSE)

  purrr::set_names(x, c("Object", "Object type", "Draw before SDM"))
})


###############################################################################
# renderUI()'s

#------------------------------------------------------------------------------
### Button widget to remove additional polygon
output$pretty_plot_addobj_remove_execute_uiOut_button <- renderUI({
  req(vals$pretty.addobj)

  actionButton("pretty_plot_addobj_remove_execute",
               "Remove selected additional objects")
})


#------------------------------------------------------------------------------
### Select widget for including other polygons
output$pretty_plot_addobj_which_uiOut_select <- renderUI({
  choices.list <- list("Import new object" = 4)

  if (isTruthy(vals$eval.data)) {
    choices.list <- c("Validation points" = 3, choices.list)
  }
  if (isTruthy(vals$overlay.land)) {
    choices.list <- c("Erasing polygon" = 2, choices.list)
  }
  if (isTruthy(vals$overlay.bound)) {
    choices.list <- c("Study area polygon" = 1, choices.list)
  }

  selectInput("pretty_plot_addobj_which", tags$h5("Add polygon to map"),
              choices = choices.list, selected = NULL)
})

#------------------------------------------------------------------------------
### Object type
output$pretty_plot_addobj_type_uiOut_radio <- renderUI({
  req(input$pretty_plot_addobj_which)

  if (input$pretty_plot_addobj_which == 4) {
    choices.list <- list("Point(s)" = 1, "Polygon(s)" = 2)
    choices.selected <- 1

  } else if (input$pretty_plot_addobj_which == 3) {
    choices.list <- list("Point(s)" = 1)
    choices.selected <- 1

  } else { #polygons
    choices.list <- list("Polygon(s)" = 2)
    choices.selected <- 2
  }

  radioButtons("pretty_plot_addobj_type", tags$h5("Object type:"),
               choices = choices.list, selected = choices.selected)
})

#------------------------------------------------------------------------------
### Point or fill color transparent checkbox
output$pretty_plot_addobj_color_ptfillcheck_uiOut_check <- renderUI({
  req(input$pretty_plot_addobj_which, input$pretty_plot_addobj_type)

  if (input$pretty_plot_addobj_which == 3) {
    input.lab <- "Make absence points transparent"

  } else {
    input.lab <- ifelse(
      input$pretty_plot_addobj_type == 1,
      "Make points transparent",
      "Make polygon fill color transparent"
    )
  }

  checkboxInput("pretty_plot_addobj_color_ptfillcheck", input.lab, value = FALSE)
})

#----------------------------------------------------------
### Point or fill color
output$pretty_plot_addobj_color_ptfill_uiOut_colour <- renderUI({
  req(input$pretty_plot_addobj_color_ptfillcheck == FALSE)

  if (input$pretty_plot_addobj_which == 3) {
    input.lab <- "Click to select present point color"

  } else {
    input.lab <- ifelse(
      input$pretty_plot_addobj_type == 1,
      "Click to select point color", "Click to select polygon fill color"
    )
  }

  input.default <- switch(
    as.numeric(input$pretty_plot_addobj_which), "red", "tan", "blue", "black"
  )

  colourpicker::colourInput(
    "pretty_plot_addobj_color_ptfill", tags$h5(input.lab),
    showColour = "background", value = input.default
  )
})


#----------------------------------------------------------
#----------------------------------------------------------
### Validation absence point or border color transparent checkbox
output$pretty_plot_addobj_color_absbordercheck_uiOut_check <- renderUI({
  req(input$pretty_plot_addobj_which, input$pretty_plot_addobj_type)

  if (input$pretty_plot_addobj_which == 4 & input$pretty_plot_addobj_type == 1) {
    NULL

  } else {
    input.lab <- ifelse(
      input$pretty_plot_addobj_which == 3,
      "Make absence points transparent", "Make polygon border(s) transparent"
    )

    checkboxInput("pretty_plot_addobj_color_absbordercheck", input.lab, value = FALSE)
  }
})

#----------------------------------------------------------
### Validation absence point or border color
output$pretty_plot_addobj_color_absborder_uiOut_colour <- renderUI({
  req(input$pretty_plot_addobj_color_absbordercheck == FALSE)

  if (input$pretty_plot_addobj_which == 4 & input$pretty_plot_addobj_type == 1) {
    NULL

  } else {
    input.lab <- ifelse(
      input$pretty_plot_addobj_which == 3,
      "Click to select absence point color",
      "Click to select polygon border color"
    )


    input.default <- switch(
      as.numeric(input$pretty_plot_addobj_which),
      "red", "black", "red", "black"
    )

    colourpicker::colourInput(
      "pretty_plot_addobj_color_absborder",
      tags$h5(input.lab),
      showColour = "background", value = input.default
    )
  }
})


#------------------------------------------------------------------------------
### Point type / line type
output$pretty_plot_addobj_pchlty_uiOut_select <- renderUI({
  req(input$pretty_plot_addobj_which, input$pretty_plot_addobj_type)

  # Set label based on which object
  input.lab <- ifelse(
    input$pretty_plot_addobj_type == 1,
    "Point type", "Line type of polygon border(s)"
  )

  # Set list of choices based on object type
  if (input$pretty_plot_addobj_type == 1) {
    choices.list <- list(
      "0: Open Square" = 0, "1: Open Circle" = 1, "2: Open Up Triangle" = 2, "3: Plus" = 3,
      "4: X" = 4, "5: Open Diamond" = 5, "6: Open Down Triangle" = 6, "7: Square with X" = 7,
      "8: Asterisk" = 8, "9: Diamond with Plus" = 9, "10: Circle with Plus" = 10,
      "11: Up-Down Triangles" = 11, "12: Square with Plus" = 12, "13: Circle with X" = 13,
      "14: Square with Up Triangle" = 14, "15: Filled Square" = 15,
      "16: Filled Circle" = 16, "17: Filled Up Triangle" = 17, "18: Filled Diamond" = 18,
      "19: Filled Large Circle" = 19, "20: Filled Small Circle" = 20
    )
    choices.selected <- 19

  } else {
    choices.list <- list(
      "Solid" = 1, "Dash" = 2, "Dot" = 3, "Dot-dash" = 4, "Long dash" = 5,
      "Dot-long dash" = 6
    )
    choices.selected <- 1
  }

  selectInput("pretty_plot_addobj_pchlty", tags$h5(input.lab),
              choices = choices.list, selected = choices.selected)
})


#------------------------------------------------------------------------------
### Point size / line width
output$pretty_plot_addobj_cexlwd_uiOut_numeric <- renderUI({
  req(input$pretty_plot_addobj_which, input$pretty_plot_addobj_type)

  input.lab <- ifelse(
    input$pretty_plot_addobj_type == 1,
    "Point size", "Line width of polygon border(s)"
  )
  input.default <- switch(
    as.numeric(input$pretty_plot_addobj_which), 1.5, 0.3, 0.5, 1
  )

  numericInput("pretty_plot_addobj_cexlwd", tags$h5(input.lab),
               value = input.default, step = 0.1)
})


###############################################################################
# User file-specific loading and processing to sf object, and their respective flags

#----------------------------------------------------------
# .csv

### Flag for successfully loaded file
output$pretty_plot_addobj_own_csv_flag <- reactive({
  isTruthy(pretty_plot_addobj_own_csv_read())
})
outputOptions(output, "pretty_plot_addobj_own_csv_flag",
              suspendWhenHidden = FALSE)

### Load and process
pretty_plot_addobj_own_csv_read <- reactive({
  file.in <- input$pretty_plot_addobj_own_csv_file
  req(file.in)

  # Ensure file extension is .csv (RStudio type, browser type)
  if (!(file.in$type %in% c("text/csv", "application/vnd.ms-excel"))) return()

  csv.data <- read.csv(file.in$datapath, stringsAsFactors = FALSE)

  return(list(csv.data, file.in$name))
})

pretty_plot_addobj_own_csv_process <- reactive({
  csv.data     <- pretty_plot_addobj_own_csv_read()[[1]]
  csv.data[csv.data == ""] <- NA
  names(csv.data) <- c("lon", "lat")

  if (input$pretty_plot_addobj_type == 1) {
    # Points
    validate(
      need(!any(is.na(csv.data)),
           paste("Error: for points, no entries in the longitude or",
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
    csv.sfc <- pts_to_sfc_coords_shiny(csv.data[, 1:2], crs.ll, FALSE)
    # ^ checks dateline and validity
  }

  validate(
    need(csv.sfc,
         paste("Error: the GUI was unable to process the provided .csv file;",
               "please ensure that the .csv file has the longitude points",
               "in the first column, the latitude points in the second",
               "column, and that the entries are valid"))
  )

  csv.sfc
})


#----------------------------------------------------------
addobj_gis_proc_shiny <- function(gis.file, obj.type) {
  gis.sfc <- st_geometry(gis.file)
  gis.file <- suppressMessages(st_union(gis.file))
  gis.sfc <- check_dateline(gis.sfc, 60, FALSE)

  validate.message <- paste(
    "Error: the GUI was unable to process the provided shapefile;",
    "please ensure that you selected the correct object type (point vs polygon)"
  )

  if (obj.type == 1) {
    if (!(any(grepl("POINT", class(gis.sfc))))) {
      gis.sfc <- try(st_cast(gis.sfc, "POINT"), silent = TRUE)
    }

    validate.check <- any(grepl("POINT", class(gis.sfc)))

  } else {
    if (!(any(grepl("POLYGON", class(gis.sfc))))) {
      gis.sfc <- try(st_cast(gis.sfc, "POLYGON"), silent = TRUE)
    }
    # Check that polygon(s) are valid
    gis.sfc <- try(check_valid(gis.sfc, progress.detail = FALSE),
                   silent = TRUE)

    validate.check <- any(grepl("POLYGON", class(gis.sfc)))
  }


  validate(
    need(isTruthy(gis.sfc) & validate.check,
         paste("Error: the GUI was unable to process the provided shapefile;",
               "please ensure that you selected the correct object type",
               "(point vs polygon)"))
  )

  gis.sfc
}

#----------------------------------------------------------
# GIS shp

### Flag for successfully loaded file
output$pretty_plot_addobj_own_shp_flag <- reactive({
  isTruthy(pretty_plot_addobj_own_shp_read())
})
outputOptions(
  output, "pretty_plot_addobj_own_shp_flag", suspendWhenHidden = FALSE
)

### Read/upload
pretty_plot_addobj_own_shp_read <- reactive({
  files.in <- req(input$pretty_plot_addobj_own_shp_files)

  withProgress(message = "Uploading object", value = 0.6, {
    gis.file.shp <- read.shp.shiny(files.in)
    incProgress(0.4)
    shp.name <- strsplit(files.in$name[1], "[.]")[[1]][1]
  })

  if (isTruthy(gis.file.shp)) list(gis.file.shp, shp.name) else NULL
})

### Process
pretty_plot_addobj_own_shp_process <- reactive({
 withProgress(message = "Processing object", value = 0.6, {
   x <- addobj_gis_proc_shiny(
     pretty_plot_addobj_own_shp_read()[[1]], input$pretty_plot_addobj_type
   )
   incProgress(0.4)
  })

  x
})


#----------------------------------------------------------
# GIS gdb

### Flag for successfully loaded file
output$pretty_plot_addobj_own_gdb_flag <- reactive({
  isTruthy(pretty_plot_addobj_own_gdb_read())
})
outputOptions(output, "pretty_plot_addobj_own_gdb_flag", suspendWhenHidden = FALSE)


### Read/upload
pretty_plot_addobj_own_gdb_read <- eventReactive(
  input$pretty_plot_addobj_own_gdb_load,
  {
    gdb.path <- input$pretty_plot_addobj_own_gdb_path
    gdb.name <- input$pretty_plot_addobj_own_gdb_name

    withProgress(message = "Uploading object", value = 0.6, {
      gis.file.gdb <- try(st_read(gdb.path, gdb.name, quiet = TRUE),
                          silent = TRUE)
      incProgress(0.6)
    })

    if (isTruthy(gis.file.gdb)) list(gis.file.gdb, gdb.name) else NULL
  })

### Process
pretty_plot_addobj_own_gdb_process <- reactive({
  withProgress(message = "Processing object", value = 0.6, {
    x <- addobj_gis_proc_shiny(
      pretty_plot_addobj_own_gdb_read()[[1]], input$pretty_plot_addobj_type
    )
    incProgress(0.4)
  })

  x
})

###############################################################################
