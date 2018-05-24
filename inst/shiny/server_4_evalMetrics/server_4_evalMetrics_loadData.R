### Load and store validation data
### Note: in functions, numeric (count) validation data is often referred to...
### ...as 'presence' and 'absence' data


###############################################################################
# Flags for conditionalPanel()'s

###########################################################
### CSV error message and inputs
output$eval_csv_1_flag <- reactive({
  !is.null(eval_data_1_csv_load())
})
outputOptions(output, "eval_csv_1_flag", suspendWhenHidden = FALSE)


###########################################################
### GIS error messages
output$eval_gis_1_shp_flag <- reactive({
  !is.null(eval_data_1_shp_load())
})
outputOptions(output, "eval_gis_1_shp_flag", suspendWhenHidden = FALSE)

output$eval_gis_1_gdb_flag <- reactive({
  !is.null(eval_data_1_gdb_load())
})
outputOptions(output, "eval_gis_1_gdb_flag", suspendWhenHidden = FALSE)


###########################################################
# Messages for invalid number/type of column selection
# Ouputs only control error messages; rendered widgets use req() statements

### CSV
output$eval_csv_1_error_flag <- reactive({
  if (input$eval_data_type_1 == 1) {
    if (length(input$eval_csv_names_1) != 3) {
      1
    } else {
      FALSE
    }
  } else {
    code.flag <- eval_data_1_csv_pacodes()
    if ("error1" %in% code.flag) {
      1
    } else if ("error2" %in% code.flag) {
      2
    } else {
      FALSE
    }
  }
})
outputOptions(output, "eval_csv_1_error_flag", suspendWhenHidden = FALSE)

### GIS
output$eval_gis_1_error_flag <- reactive({
  if (input$eval_data_type_1 == 1) {
    FALSE
  } else {
    code.flag <- eval_data_1_gis_pacodes()
    if ("error2" %in% code.flag) { #"error2" to keep consistent with csv
      2
    } else {
      FALSE
    }
  }
})
outputOptions(output, "eval_gis_1_error_flag", suspendWhenHidden = FALSE)


###########################################################
### Display GIS 'general' input if data has been loaded from shp and...
# ...shp is selected, or if same for gdb
output$eval_gis_1_flag <- reactive({
  req(length(vals$eval.data.gis.file.1) != 0)

  x <- try((!is.null(eval_data_1_shp_load()) & input$eval_load_type_1 == 2 &
              vals$eval.data.gis.file.1[[2]] == 2),
           silent = TRUE)
  y <- try((!is.null(eval_data_1_gdb_load()) & input$eval_load_type_1 == 3 &
              vals$eval.data.gis.file.1[[2]] == 3),
           silent = TRUE)

  isTruthy(x) | isTruthy(y)
})
outputOptions(output, "eval_gis_1_flag", suspendWhenHidden = FALSE)


###############################################################################
###############################################################################
# Loading validation data from csv

### Read in .csv file
eval_data_1_csv_load <- reactive({
  file.in <- input$eval_csv_1
  req(file.in)

  if (!(file.in$type %in% c("text/csv", "application/vnd.ms-excel"))) return()

  csv.data <- read.csv(file.in$datapath, stringsAsFactors = FALSE)
  return (list(file.in$name, csv.data))
})

### Get options for presence/absence codes
eval_data_1_csv_pacodes <- reactive({
  csv.info <- eval_data_1_csv_load()
  req(csv.info, input$eval_data_type_1 == 2)

  if (length(input$eval_csv_names_1) != 3) return("error1")

  col.pa <- as.numeric(input$eval_csv_names_1[3])
  choice.input.names <- unique(csv.info[[2]][, col.pa])
  choice.input.names <- choice.input.names[order(choice.input.names)]

  if (length(choice.input.names) > 50) return("error2")

  choice.input.names
})

### CSV: Process and then save validation data to reactiveVal
eval_data_1_csv <- eventReactive(input$eval_csv_execute_1, {
  req(eval_data_1_csv_load(), length(input$eval_csv_names_1) == 3)

  # Prep for processing
  data.type <- as.numeric(input$eval_data_type_1)
  csv.all <- eval_data_1_csv_load()[[2]]
  csv.selected <- csv.all[, as.numeric(input$eval_csv_names_1)]

  # Process data.frame with selected data and create sf object
  vals$eval.data.list <- eval_proc_df(
    csv.selected, data.type, input$eval_csv_codes_1_p, input$eval_csv_codes_1_a
  )
  vals$eval.data.specs <- data.type

  "Loaded and stored validation data"
})


###############################################################################
###############################################################################
# Loading validation data from GIS

###########################################################
### Load validation shp and save it to reactiveValue
eval_data_1_shp_load <- reactive({
  req(input$eval_gis_shp_1)

  withProgress(message = "Loading GIS file", value = 0.3, {
    gis.file.shp <- read.shp.shiny(input$eval_gis_shp_1)
    gis.file.success <- isTruthy(gis.file.shp)
    incProgress(0.4)

    if (gis.file.success) {
      validate(
        need(inherits(st_geometry(gis.file.shp), "sfc_POINT"),
             paste("Error: Please ensure that the uploaded shapefile",
                   "only contains points")) %then%
          need(!is.na(st_crs(gis.file.shp)$proj4string),
               paste("Error: The uploaded shapefile does not have a",
                     "defined coordinate system"))
      )

      gis.file.shp <- check_dateline(gis.file.shp)

      if (!identical(st_crs(gis.file.shp), crs.ll)) {
        gis.file.shp.ll <- st_transform(gis.file.shp)
      } else {
        gis.file.shp.ll <- gis.file.shp
      }
    }
    incProgress(0.3)
  })

  if (!gis.file.success) {
    NULL
  } else {
    gis.file.shp.ll
  }
})

observe({
  req(eval_data_1_shp_load())
  vals$eval.data.gis.file.1 <- list(eval_data_1_shp_load(), 2)
})


###########################################################
### Load validation gdb and save it to reactiveValue
eval_data_1_gdb_load <- eventReactive(input$eval_gis_gdb_load_1, {
  gdb.path <- input$eval_gis_gdb_path_1
  gdb.name <- input$eval_gis_gdb_name_1

  withProgress(message = "Loading GIS file", value = 0.3, {
    gis.file.gdb <- try(st_read(gdb.path, gdb.name, quiet = TRUE),
                        silent = TRUE)
    gis.file.success <- isTruthy(gis.file.gdb)
    incProgress(0.4)

    if (gis.file.success) {
      validate(
        need(inherits(st_geometry(gis.file.gdb), "sfc_POINT"),
             paste("Error: Please ensure that the uploaded .gdb file",
                   "only contains points")) %then%
          need(!is.na(st_crs(gis.file.gdb)$proj4string),
               paste("Error: The uploaded .gdb file does not have a",
                     "defined coordinate system"))
      )

      gis.file.gdb <- check_dateline(gis.file.gdb)

      if (!identical(st_crs(gis.file.gdb), crs.ll)) {
        gis.file.gdb.ll <- st_transform(gis.file.gdb)
      } else {
        gis.file.gdb.ll <- gis.file.gdb
      }
    }
    incProgress(0.3)
  })

  if (!gis.file.success) {
    NULL
  } else {
    gis.file.gdb.ll
  }
})

observe({
  req(eval_data_1_gdb_load())
  vals$eval.data.gis.file.1 <- list(eval_data_1_gdb_load(), 3)
  shinyjs::reset("eval_gis_shp_1")
})


###########################################################
### Get options for presence/absence codes
eval_data_1_gis_pacodes <- reactive({
  pa.sf.list <- vals$eval.data.gis.file.1
  req(length(pa.sf.list) > 0)
  pa.sf <- pa.sf.list[[1]]
  req(pa.sf, pa.sf.list[[2]] == input$eval_load_type_1)

  col.pa <- as.numeric(input$eval_gis_names_1)
  choice.input.names <- unique(st_set_geometry(pa.sf, NULL)[, col.pa])

  if (length(choice.input.names) > 50) return("error2")
  # "error2' to keep consistent with csv error code

  sort(choice.input.names)
})

###########################################################
### GIS: process and then save validation data to reactiveVar
eval_data_1_gis <- eventReactive(input$eval_gis_execute_1, {
  req(length(vals$eval.data.gis.file.1) > 0)

  # Prep for processing
  data.type <- as.numeric(input$eval_data_type_1)
  pa.sf <- vals$eval.data.gis.file.1[[1]]
  x <- do.call(rbind, st_geometry(pa.sf))
  gis.selected <- data.frame(
    lon = x[, 1], lat = x[, 2],
    z = st_set_geometry(pa.sf, NULL)[, as.numeric(input$eval_gis_names_1)]
  )

  # Process data.frame with selected data and create sf object
  vals$eval.data.list <- eval_proc_df(
    gis.selected, data.type, input$eval_gis_codes_1_p, input$eval_gis_codes_1_a
  )
  vals$eval.data.specs <- data.type

  "Loaded and stored validation data"
})


###############################################################################
