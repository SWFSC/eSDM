# Import and store validation data
# Note: in functions, numeric (count) validation data is often referred to
#   as 'presence' and 'absence' data


###############################################################################
# Flags for conditionalPanel()'s

###########################################################
### .csv error message and inputs
output$eval_csv_flag <- reactive(isTruthy(eval_data_csv_load()))
outputOptions(output, "eval_csv_flag", suspendWhenHidden = FALSE)


###########################################################
### GIS error messages
output$eval_gis_shp_flag <- reactive(isTruthy(eval_data_shp_load()))
outputOptions(output, "eval_gis_shp_flag", suspendWhenHidden = FALSE)

output$eval_gis_gdb_flag <- reactive(isTruthy(eval_data_gdb_load()))
outputOptions(output, "eval_gis_gdb_flag", suspendWhenHidden = FALSE)


###########################################################
# Messages for invalid number/type of column selection
# Ouputs only control error messages; rendered widgets use req() statements

### .csv
output$eval_csv_error_flag <- reactive({
  if (input$eval_data_type == 1) {
    if (length(input$eval_csv_names) != 3) 1 else FALSE

  } else {
    code.flag <- eval_data_csv_pacodes()
    if ("error1" %in% code.flag) {
      1
    } else if ("error2" %in% code.flag) {
      2
    } else {
      FALSE
    }
  }
})
outputOptions(output, "eval_csv_error_flag", suspendWhenHidden = FALSE)

### GIS
output$eval_gis_error_flag <- reactive({
  if (input$eval_data_type == 1) {
    FALSE

  } else {
    code.flag <- eval_data_gis_pacodes()
    #"error2" to keep consistent with csv
    if ("error2" %in% code.flag) 2 else FALSE
  }
})
outputOptions(output, "eval_gis_error_flag", suspendWhenHidden = FALSE)


###########################################################
### Display GIS 'general' input if data has been loaded from shp and
###   shp is selected, or if same for gdb
output$eval_gis_flag <- reactive({
  req(length(vals$eval.data.gis.info) != 0)

  x <- try(
    isTruthy(eval_data_shp_load()) & input$eval_load_type == 2 & vals$eval.data.gis.info[[3]] == 2,
    silent = TRUE
  )
  y <- try(
    isTruthy(eval_data_gdb_load()) & input$eval_load_type == 3 & vals$eval.data.gis.info[[3]] == 3,
    silent = TRUE
  )

  isTruthy(x) | isTruthy(y)
})
outputOptions(output, "eval_gis_flag", suspendWhenHidden = FALSE)


###############################################################################
###############################################################################
# Importing validation data from .csv

### Read in .csv file
eval_data_csv_load <- reactive({
  file.in <- input$eval_csv
  req(file.in)

  if (!(file.in$type %in% c("text/csv", "application/vnd.ms-excel"))) return()

  csv.data <- read.csv(file.in$datapath, stringsAsFactors = FALSE)
  list(file.in$name, csv.data)
})

### Get options for presence/absence codes
eval_data_csv_pacodes <- reactive({
  csv.info <- eval_data_csv_load()
  req(csv.info, input$eval_data_type == 2)

  if (length(input$eval_csv_names) != 3) return("error1")

  col.pa <- as.numeric(input$eval_csv_names[3])
  choice.input.names <- na.omit(unique(csv.info[[2]][, col.pa]))

  if (length(choice.input.names) > 50) return("error2")

  sort(choice.input.names)
})

### .csv: Process and then save validation data to reactiveVal
eval_data_csv <- eventReactive(input$eval_csv_execute, {
  req(eval_data_csv_load(), length(input$eval_csv_names) == 3)

  vals$eval.data <- NULL
  vals$eval.data.specs <- NULL

  # Prep for processing
  data.type <- as.numeric(input$eval_data_type)
  csv.all <- eval_data_csv_load()[[2]]
  csv.df <- csv.all[, as.numeric(input$eval_csv_names)]
  csv.df.na <- which(is.na(csv.df[, 3]))
  if (length(csv.df.na) > 0) csv.df <- csv.df[-csv.df.na, ]

  # Process data.frame with selected data and create sf object
  vals$eval.data <- eval_proc_df(
    csv.df, data.type, input$eval_csv_codes_p, input$eval_csv_codes_a
  )
  vals$eval.data.specs <- c(eval_data_csv_load()[[1]], data.type)

  ifelse(
    length(csv.df.na) > 0,
    paste("Imported validation data;", length(csv.df.na),
          "points were removed because they had values of NA in the",
          "validation data column"),
    "Imported validation data"
  )
})


###############################################################################
###############################################################################
# Importing validation data from GIS

###########################################################
### Load validation shp and save it to reactiveValue
eval_data_shp_load <- reactive({
  req(input$eval_gis_shp)

  withProgress(message = "Processing GIS file", value = 0.3, {
    gis.file.shp <- read.shp.shiny(input$eval_gis_shp)
    gis.file.success <- isTruthy(gis.file.shp)
    incProgress(0.4)

    if (gis.file.success) {
      gis.name <- strsplit(input$eval_gis_shp$name[1], "\\.")[[1]][1]

      gis.file.shp <- check_dateline(gis.file.shp)
      gis.file.shp.ll <- st_transform(gis.file.shp, crs.ll)
    }
    incProgress(0.3)
  })

  if (!gis.file.success) {
    NULL
  } else {
    list(gis.name, gis.file.shp.ll)
  }
})

observe({
  req(eval_data_shp_load())
  vals$eval.data.gis.info <- c(eval_data_shp_load(), 2)
})


###########################################################
### Load validation gdb and save it to reactiveValue
eval_data_gdb_load <- eventReactive(input$eval_gis_gdb_load, {
  gdb.path <- input$eval_gis_gdb_path
  gdb.name <- input$eval_gis_gdb_name

  withProgress(message = "Processing GIS file", value = 0.3, {
    gis.file.gdb <- try(
      st_read(gdb.path, gdb.name, quiet = TRUE), silent = TRUE
    )
    gis.file.success <- isTruthy(gis.file.gdb)
    incProgress(0.4)

    if (gis.file.success) {
      gis.file.gdb <- check_dateline(gis.file.gdb)
      gis.file.gdb.ll <- st_transform(gis.file.gdb, crs.ll)
    }
    incProgress(0.3)
  })

  if (!gis.file.success) {
    NULL
  } else {
    list(gdb.name, gis.file.gdb.ll)
  }
})

observe({
  req(eval_data_gdb_load())
  vals$eval.data.gis.info <- c(eval_data_gdb_load(), 3)
  shinyjs::reset("eval_gis_shp")
})


###########################################################
### Get options for presence/absence codes
eval_data_gis_pacodes <- reactive({
  req(vals$eval.data.gis.info)
  req(vals$eval.data.gis.info[[3]] == input$eval_load_type)

  pa.sf <- vals$eval.data.gis.info[[2]]
  col.pa <- as.numeric(req(input$eval_gis_names))
  choice.input.names <- na.omit(unique(st_set_geometry(pa.sf, NULL)[, col.pa]))

  if (length(choice.input.names) > 50) return("error2")
  # "error2' to keep consistent with csv error code

  sort(choice.input.names)
})

###########################################################
### GIS: process and then save validation data to vals
eval_data_gis <- eventReactive(input$eval_gis_execute, {
  req(vals$eval.data.gis.info)

  vals$eval.data <- NULL
  vals$eval.data.specs <- NULL

  x <- vals$eval.data.gis.info[[2]]

  # Validate checks
  validate(
    need(inherits(st_geometry(x), "sfc_POINT"),
         paste("Error: Please ensure that the uploaded object",
               "only consists of points")) %then%
      need(!is.na(st_crs(x)$proj4string),
           paste("Error: The uploaded object does not have a",
                 "defined coordinate system"))
  )

  # Prep for processing
  data.type <- as.numeric(input$eval_data_type)
  x.coords <- do.call(rbind, st_geometry(x))
  x.df <- data.frame(
    lon = x.coords[, 1], lat = x.coords[, 2],
    z = st_set_geometry(x, NULL)[, as.numeric(input$eval_gis_names)]
  )
  x.df.na <- which(is.na(x.df$z))
  if (length(x.df.na) > 0) x.df <- x.df[-x.df.na, ]

  # Process data.frame with selected data and create sf object
  vals$eval.data <- eval_proc_df(
    x.df, data.type, input$eval_gis_codes_p, input$eval_gis_codes_a
  )
  vals$eval.data.specs <- c(vals$eval.data.gis.info[[1]], data.type)

  ifelse(
    length(x.df.na) > 0,
    paste("Imported validation data;", length(x.df.na),
          "points were removed because they had values of NA in the",
          "validation data column"),
    "Imported validation data"
  )
})


###############################################################################
### Generate table with validation data stats
table_eval_pts_filename <- reactive({
  eval.data <- vals$eval.data
  data.type <- vals$eval.data.specs[2]
  req(inherits(eval.data, "sf"), data.type)

  paste("Filename:", vals$eval.data.specs[1])
})

table_eval_pts <- reactive({
  eval.data <- vals$eval.data
  data.type <- vals$eval.data.specs[2]
  req(inherits(eval.data, "sf"), data.type)

  data.type.txt <- ifelse(data.type == 1, "Count", "Presence/absence")
  pres.num <- sum(eval.data$sight == 1)
  abs.num <- sum(eval.data$sight == 0)

  if (data.type == 1) {
    pres.data <- eval.data %>% dplyr::filter(sight == 1)
    count.range <- ifelse(
      length(unique(pres.data$count)) == 0,
      NA,
      paste(range(round(pres.data$count, 2)), collapse = " to ")
    )

    data.frame(
      c("Data type:", "Number of points with non-zero counts:",
        "Number of points with counts of 0:", "Range of non-zero counts:"),
      c(data.type.txt, pres.num, abs.num, count.range),
      stringsAsFactors = FALSE
    )

  } else if (data.type == 2) {
    data.frame(
      c("Data type:", "Number of presence points:", "Number of absence points:"),
      c(data.type.txt, pres.num, abs.num),
      stringsAsFactors = FALSE
    )

  } else {
    validate("Error in evaluation metric table creation")
  }
})

###############################################################################
