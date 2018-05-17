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

output$read_model_csv_flag <- reactive({
  !is.null(read_model_csv())
})
outputOptions(output, "read_model_csv_flag", suspendWhenHidden = FALSE)


###############################################################################
# Reactive function for csv renderUI's

### Name of data and if(applicable) weight column selected by user
model_csv_names_selected <- reactive({
  data.names <- names(read_model_csv()[[2]])
  data.selected <- c(data.names[as.numeric(input$model_csv_names_pred)], NA, NA)

  # error.idx <- as.numeric(input$model_csv_names_error) - 1
  weight.idx <- as.numeric(input$model_csv_names_weight) - 1
  # if (error.idx != 0) data.selected[2] <- data.names[error.idx]
  if (weight.idx != 0) data.selected[3] <- data.names[weight.idx]

  list(data.selected)
})

### Identify row indices of NA values in given Prediction column
model_csv_NA_idx <- reactive({
  req(input$model_csv_names_pred)
  data.csv <- read_model_csv()[[2]]

  data.csv.pred <- data.csv[, as.numeric(input$model_csv_names_pred)]

  na_which(data.csv[, as.numeric(input$model_csv_names_pred)])
})

### Get column names of csv data
csv_names_choice_input <- reactive({
  choice.input.names <- names(read_model_csv()[[2]])
  choice.input <- seq_along(choice.input.names)
  names(choice.input) <- choice.input.names

  choice.input
})


###############################################################################
###############################################################################

# Load and process data from csv file, and then add relevant data to vals
create_sf_csv <- eventReactive(input$model_create_csv, {
  withProgress(message = "Loading SDM from .csv file", value = 0.3, {
    #######################################################
    ### Load and process data and input variables
    lon.idx <- as.numeric(input$model_csv_names_lon)
    lat.idx <- as.numeric(input$model_csv_names_lat)
    pred.idx <- as.numeric(input$model_csv_names_pred)
    error.idx <- NA #as.numeric(input$model_csv_names_error)
    weight.idx <- as.numeric(input$model_csv_names_weight)

    error.idx <- NA #ifelse(error.idx == 1, NA, error.idx - 1)
    weight.idx <- ifelse(weight.idx == 1, NA, weight.idx - 1)

    csv.idx <- c(lon.idx, lat.idx, pred.idx, error.idx, weight.idx)

    csv.all <- read_model_csv()[[2]]
    csv.data <- cbind(csv.all[, csv.idx[1:3]], NA, NA)
    if (!is.na(csv.idx[4])) csv.data[, 4] <- csv.all[csv.idx[4]]
    if (!is.na(csv.idx[5])) csv.data[, 5] <- csv.all[csv.idx[5]]

    # Change invalid densities to NA
    na.idx <- model_csv_NA_idx()
    if (!anyNA(na.idx)) csv.data[na.idx, 3:5] <- NA

    # Sort data by lat (primary) then long for bottom up sort
    csv.data <- data_sort(csv.data, 2, 1)

    # Remove lat/long; add 'Pixel' column to keep track of indices
    csv.data <- cbind(csv.data, 1:nrow(csv.data))
    names(csv.data) <- c("Lon", "Lat", "Pred", "Error", "Weight", "Pixels")


    #######################################################
    # Create sf object

    #####################################
    ### a) Initial check to see if there are any obvious data issues
    validate(
      need(!anyNA(csv.data$Lon),
           paste("Error: At least one of the points in the longitude data",
                 "column has a value of 'NA'")),
      need(!anyNA(csv.data$Lat),
           paste("Error: At least one of the points in the latitude data",
                 "column has a value of 'NA'"))
    )

    diff.lon <-
      max(csv.data$Lon, na.rm = TRUE) - min(csv.data$Lon, na.rm = TRUE)
    validate(
      need(diff.lon <= 360,
           paste("Error: The longitude points in the provided Excel .csv file",
                 "have a range greater than 360 degress")),
      need(all(csv.data$Lat >= -90),
           paste("Error: All latitude values in the provided Excel .csv file",
                 "must be greater than or equal to -90 degrees")),
      need(all(csv.data$Lat <= 90),
           paste("Error: The latitude points in the provided Excel .csv file",
                 "must be less than or equal to 90 degrees"))
    )


    #####################################
    ### b) Get cell size and then adjust points to center of grid cells (if nec)
    # Get cell size
    table.l <- table(round(diff(sort(csv.data$Lon)), 5))
    table.w <- table(round(diff(sort(csv.data$Lat)), 5))

    test1 <- length(table.l) == 2 & length(table.w) == 2
    test2 <- "0" %in% names(table.l) & "0" %in% names(table.w)
    test3 <- as.numeric(names(table.l[2])) == as.numeric(names(table.w[2]))

    validate(need(all(c(test1, test2, test3)), "Not lat/long regular"))
    cell.lw <- as.numeric(names(table.w[2]))
    rm(table.l, table.w, test1, test2, test3)

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
        validate(
          need(FALSE, "Error: create_csv_grid() point location code")
        )
      }

      # Adjust points
      csv.data$Lon <- csv.data$Lon + adj.lon
      csv.data$Lat <- csv.data$Lat + adj.lat
    }


    #####################################
    ### c) Convert points to a list of sfc_POLYGONs and then to a sf object

    # If all lon points are between 180 and 360, shift them to -180 to 180 range
    #   Otherwise fix datealine issue after createing sf object
    # range.lon <- range(csv.data$Lon)
    # if ((range.lon[[1]] - cell.lw) > 180 & (range.lon[[2]] + cell.lw) > 180) {
    #   csv.data$Lon <- ifelse(csv.data$Lon > 180, csv.data$Lon - 360,
    #                          csv.data$Lon)
    # }

    # Make sf object
    incProgress(0.15)
    sfc.list <- apply(csv.data[, c("Lon", "Lat")], 1, function(i, j) {
      st_sfc(st_polygon(list(matrix(
        c(i[1] + j, i[1] - j, i[1] - j, i[1] + j, i[1] + j,
          i[2] + j, i[2] + j, i[2] - j, i[2] - j, i[2] + j),
        ncol = 2))))
    }, j = (cell.lw / 2))
    incProgress(0.3)

    sfc.poly <- st_sfc(do.call(rbind, sfc.list))
    sf.load.ll <- st_sf(csv.data[, -c(1:2)], sfc.poly, crs = crs.ll,
                        agr = "constant")

    # Ensure that sf object is in -180 to 180 longitude range
    if (st_bbox(sf.load.ll)[3] > 180) {
      incProgress(0.05, detail = "Polygon(s) span dateline; handling now")
      sf.load.ll <- st_wrap_dateline(sf.load.ll)
    }


    #####################################
    ### d) Perform final quality checks
    incProgress(detail = "Checking if model polygons are valid")
    if (!all(st_is_valid(sf.load.ll))) {
      incProgress(detail = "Making model polygons valid")
      sf.load.ll <- poly_valid_check(sf.load.ll)
    }

    sf.bbox <- as.numeric(st_bbox(sf.load.ll))
    validate(
      need(all(c(sf.bbox[3:4] <= c(180, 90), sf.bbox[1:2] >= c(-180, -90))),
           paste("Error: There was an issue processing this .csv data;",
                 "please manually ensure that all longitude and latitude values",
                 "are between [-180, 180] and [-90, 90], respectively"))
    )


    #######################################################
    ### Prep for and run function that adds relevant data to vals
    incProgress(0.2, detail = "Finishing model processing")
    model.res <- paste(cell.lw, "degrees")

    pred.type <- input$model_csv_pred_type
    model.name <- read_model_csv()[[1]]
    data.names <- model_csv_names_selected()


    #######################################################
    #### Code common to csv, raster, and gis_shp/gis_gdb functions ####
    source(file.path("server_1_loadModels",
                     "server_1_loadModels_create_local.R"),
           local = TRUE, echo = FALSE, chdir = TRUE)


    "Model predictions loaded from csv"
  })
})
