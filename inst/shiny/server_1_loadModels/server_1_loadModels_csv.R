### Code for reading in/loading .csv predictions
### Assumes column 1 is lat, column2 is long, and then user selects data column


###############################################################################
### Read in selected csv file
read_model_csv <- reactive({
  req(input$model_csv_file)
  file.in <- input$model_csv_file

  # Ensure file extension is .csv (RStudio type, browser type)
  if(!(file.in$type %in% c("text/csv", "application/vnd.ms-excel"))) return()

  return(list(file.in$name, read.csv(file.in$datapath)))
})

# observe(print(create_sf_csv()))

output$read_model_csv_flag <- reactive({
  !is.null(read_model_csv())
})
outputOptions(output, "read_model_csv_flag", suspendWhenHidden = FALSE)


###############################################################################
# Reactive function for csv renderUI's

### Get column names of csv data
csv_names_choice_input <- reactive({
  choice.input.names <- names(read_model_csv()[[2]])
  choice.input <- seq_along(choice.input.names)
  names(choice.input) <- choice.input.names

  choice.input
})

### Name of selected prediction and (if appl) uncertainty and weight column(s)
model_csv_names_selected <- reactive({
  data.names <- names(read_model_csv()[[2]])
  var.idx <- as.numeric(input$model_csv_names_var) - 1
  weight.idx <- as.numeric(input$model_csv_names_weight) - 1

  list(
    c(data.names[as.numeric(input$model_csv_names_pred)],
      ifelse(var.idx == 0, NA, data.names[var.idx]),
      ifelse(weight.idx == 0, NA, data.names[weight.idx]))
  )
})

### Identify row indices of NA values in given prediction column
model_csv_NA_idx_pred <- reactive({
  data.csv <- req(read_model_csv())[[2]]
  data.col <- as.numeric(req(input$model_csv_names_pred))
  req(data.col <= ncol(data.csv))

  na_which(data.csv[, data.col])
})

### Identify row indices of NA values in given uncertainty column
model_csv_NA_idx_var <- reactive({
  data.csv <- read_model_csv()[[2]]
  var.col <- as.numeric(req(input$model_csv_names_var))
  req((var.col - 1) <= ncol(data.csv))

  if (var.col > 1) na_which(data.csv[, (var.col - 1)]) else NA
})

### Identify row indices of NA values in given weight column
model_csv_NA_idx_weight <- reactive({
  data.csv <- read_model_csv()[[2]]
  weight.col <- as.numeric(req(input$model_csv_names_weight))
  req((weight.col - 1) <= ncol(data.csv))

  if (weight.col > 1) na_which(data.csv[, (weight.col - 1)]) else NA
})


###############################################################################
###############################################################################
# Load and process data from csv file, and then add relevant data to vals

###############################################################################
# Create data frame for original predictions
create_sf_csv_data <- reactive({
  ### Upload and process data and input variables
  lon.idx <- as.numeric(input$model_csv_names_lon)
  lat.idx <- as.numeric(input$model_csv_names_lat)
  pred.idx <- as.numeric(input$model_csv_names_pred)

  var.idx <- as.numeric(input$model_csv_names_var) - 1
  var.idx <- ifelse(var.idx == 0, NA, var.idx)
  weight.idx <- as.numeric(input$model_csv_names_weight) - 1
  weight.idx <- ifelse(weight.idx == 0, NA, weight.idx)

  csv.idx <- c(lon.idx, lat.idx, pred.idx, var.idx, weight.idx)

  csv.all <- read_model_csv()[[2]]
  csv.data <- cbind(csv.all[, csv.idx[1:3]], as.numeric(NA), as.numeric(NA))
  if (!is.na(var.idx)) csv.data[, 4] <- csv.all[, var.idx]
  if (!is.na(weight.idx)) csv.data[, 5] <- csv.all[, weight.idx]

  # Check that pred and weight data are valid
  csv.data <- check_pred_var_weight(
    csv.data, 3, ifelse(var.idx == 0, NA, 4), ifelse(weight.idx == 0, NA, 5),
    model_csv_NA_idx_pred(), model_csv_NA_idx_var(),
    model_csv_NA_idx_weight()
  )

  # Sort data by lat (primary) then lon; add idx column
  csv.data %>%
    purrr::set_names(c("Lon", "Lat", "Pred", "SE", "Weight")) %>%
    dplyr::arrange(Lat, Lon) %>%
    dplyr::mutate(idx = seq_along(Lon))
})


###############################################################################
# Create geometry of predictions from centroid coordinates
create_sf_csv_sfc <- reactive({
  withProgress(message = "Creating geometry for .csv predictions", value = 0.3, {
    #--------------------------------------------------------------------------
    lon.idx <- as.numeric(input$model_csv_names_lon)
    lat.idx <- as.numeric(input$model_csv_names_lat)

    csv.data <- read_model_csv()[[2]] %>%
      dplyr::select(Lon = !!lon.idx, Lat = !!lat.idx) %>%
      dplyr::arrange(Lat, Lon) %>%
      dplyr::mutate(idx = seq_along(Lon))


    #--------------------------------------------------------------------------
    # Create sfc object

    #------------------------------------------------------
    ### a) Initial check to see if there are any obvious data issues
    validate(
      need(!(lon.idx == lat.idx),
           paste("Error: The longitude and latitude data columns",
                 "cannot be the same")) %then%
        need(!anyNA(csv.data$Lon),
             paste("Error: At least one of the points in the longitude data",
                   "column has a value of 'NA'")),
      need(!anyNA(csv.data$Lat),
           paste("Error: At least one of the points in the latitude data",
                 "column has a value of 'NA'"))
    )

    diff.lon <- max(csv.data$Lon, na.rm = TRUE) -
      min(csv.data$Lon, na.rm = TRUE)
    validate(
      need(diff.lon <= 360,
           paste("Error: The longitude points in the provided Excel .csv file",
                 "have a range greater than 360 degress")),
      need(all(dplyr::between(csv.data$Lat, -90, 90)),
           paste("Error: All latitude values in the provided Excel .csv file",
                 "must be greater than or equal to -90 degrees and",
                 "less than or equal to 90 degree"))
    )


    #------------------------------------------------------
    ### b) Get cell size and (if nec) adjust points to center of grid cells
    # Get cell size
    table.l <- table(round(diff(sort(csv.data$Lon)), 5))
    table.w <- table(round(diff(sort(csv.data$Lat)), 5))

    # Test for if points are lat/long regular
    test1 <- length(table.l) == 2 & length(table.w) == 2
    test2 <- as.numeric(names(table.l[2])) == as.numeric(names(table.w[2]))

    validate(
      need(all(test1, test2),
           paste("Error: The points in the .csv file are not lat/long regular;",
                 "note that the longitude spacing must be the same",
                 "as the latitude spacing.",
                 "See the manual for more details about file requirements"))
    )
    cell.lw <- as.numeric(names(table.l[2]))
    rm(table.l, table.w, test1, test2)

    # Adjust points if necessary
    pt.loc <- input$model_csv_pt_loc
    if (pt.loc != 1) {
      if (pt.loc == 2) {
        adj.lon <-  cell.lw / 2
        adj.lat <- -cell.lw / 2
      } else if (pt.loc == 3) {
        adj.lon <- -cell.lw / 2
        adj.lat <- -cell.lw / 2
      } else if (pt.loc == 4) {
        adj.lon <- -cell.lw / 2
        adj.lat <-  cell.lw / 2
      } else if (pt.loc == 5) {
        adj.lon <- cell.lw / 2
        adj.lat <- cell.lw / 2
      } else {
        validate("Error: create_csv_grid() point location code")
      }

      # Adjust points to center of the polygon
      csv.data$Lon <- csv.data$Lon + adj.lon
      csv.data$Lat <- csv.data$Lat + adj.lat
    }


    #------------------------------------------------------
    ### c) Convert points to sfc object
    # If coords are all in range 180-360, convert them to -180 to 180 range
    if ((min(csv.data$Lon, na.rm = TRUE) - (cell.lw / 2)) > 180) {
      csv.data$Lon <- csv.data$Lon - 360
    }

    # Make sf object
    sfc.poly <- try(
      eSDM::pts2poly_centroids(csv.data[, c("Lon", "Lat")], cell.lw / 2, crs = crs.ll),
      silent = TRUE
    )
    validate(
      need(inherits(sfc.poly, "sfc_POLYGON"),
           paste("Error: The longitude and latitude data from the",
                 "provided CSV file could not be processed"))
    )
    incProgress(0.3)


    #------------------------------------------------------
    ### d) Perform final checks
    # Ensure that sfc object is in -180 to 180 longitude range
    # No check_valid() needed here
    sfc.poly <- check_dateline(sfc.poly, progress.detail = TRUE)
    incProgress(0.3)


    #--------------------------------------------------------------------------
    list(sfc.poly, cell.lw)
  })
})


###############################################################################
# Create sf object from data and sfc object
create_sf_csv <- eventReactive(input$model_create_csv, {
  csv.data <- create_sf_csv_data()
  csv.sfc <- create_sf_csv_sfc()[[1]]

  # Combine data df and sfc object
  withProgress(message = "Importing predictions from .csv file", value = 0.6, {
    if (nrow(csv.data) == length(csv.sfc)) {
      sf.load.ll <- st_sf(
        csv.data[, 3:6], geometry = csv.sfc, agr = "constant"
      )

    } else {
      validate("Error in creating sf object from CSV file")
    }

    ### Prep for and run function that adds relevant data to vals
    incProgress(0.4, detail = "Finishing model processing")
    model.res <- paste(create_sf_csv_sfc()[[2]], "degrees")

    pred.type <- input$model_csv_pred_type
    var.type  <- input$model_csv_var_type
    model.name <- read_model_csv()[[1]]
    data.names <- model_csv_names_selected()

    ###### Code common to all importing functions ######
    source(
      file.path("server_1_loadModels", "server_1_loadModels_create_local.R"),
      local = TRUE, echo = FALSE, chdir = TRUE
    )
    ####################################################

    "Predictions imported from CSV file"
  })
})
