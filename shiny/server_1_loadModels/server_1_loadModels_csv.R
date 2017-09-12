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
  
  error.idx <- as.numeric(input$model_csv_names_error) - 1
  weight.idx <- as.numeric(input$model_csv_names_weight) - 1
  if (error.idx != 0) data.selected[2] <- data.names[error.idx]
  if (weight.idx != 0) data.selected[3] <- data.names[weight.idx]
  
  list(data.selected)
})

### Identify row indices of NA values in given Prediction column
model_csv_NA_idx <- reactive({
  req(input$model_csv_names_pred)
  data.csv <- read_model_csv()[[2]]
  
  data.csv.pred <- data.csv[,as.numeric(input$model_csv_names_pred)]
  
  na.which(data.csv[,as.numeric(input$model_csv_names_pred)])
})

### Get column names of csv data
csv_names_choice_input <- reactive({
  choice.input.names <- names(read_model_csv()[[2]])
  choice.input <- seq_along(choice.input.names)
  names(choice.input) <- choice.input.names
  
  choice.input
})


###############################################################################
# Load and process data from csv file

### Create underlying grid for csv predictions
create_spdf_csv_grid <- reactive({
  lon.idx <- as.numeric(input$model_csv_names_lon)
  lat.idx <- as.numeric(input$model_csv_names_lat)
  
  csv.all <- read_model_csv()[[2]]
  csv.toSPDF <- cbind(csv.all[,c(lon.idx, lat.idx)], NA, NA, NA)

  # Sort data by lat (primary) then long for bottom up sort
  csv.toSPDF <- data.sort(csv.toSPDF, 2, 1)
  
  # Add 'Pixel' column to keep track of indices
  csv.toSPDF <- cbind(csv.toSPDF, 1:nrow(csv.toSPDF))
  names(csv.toSPDF) <- c("Long", "Lat", "Pred", "Error", "Weight", "Pixels")
  
  ### Create spatial objects
  #     Need all predictions to be on -180 to 180 long scale
  #     First check: are all points > 0? Then subtract 360 from longs > 180
  if(all(csv.toSPDF$Long > 0)) {
    csv.toSPDF$Long <- ifelse(csv.toSPDF$Long > 180, csv.toSPDF$Long - 360, 
                              csv.toSPDF$Long)
  }
  spdf.pts <- csv.toSPDF
  
  # Try to coerce SpatialPoints to SpatialPixels
  coordinates(spdf.pts) <- ~Long+Lat
  proj4string(spdf.pts) <- crs.ll
  spdf.pix <- try(as(spdf.pts, "SpatialPixelsDataFrame"))
  validate(
    need(class(spdf.pix) != "try-error", "Provided csv points are not lat-long regular")
  )
  
  # Adjust provided points to center of grid cells
  pt.loc <- input$model_csv_pt_loc
  if(pt.loc != 1) {
    cell.size.lon <- round(unname(spdf.pix@grid@cellsize["Long"]), 5)
    cell.size.lat <- round(unname(spdf.pix@grid@cellsize["Lat"]), 5)
    if(pt.loc == 2) {
      adj.lon <-  cell.size.lon / 2
      adj.lat <- -cell.size.lat / 2
    }
    if(pt.loc == 3) {
      adj.lon <- -cell.size.lon / 2
      adj.lat <- -cell.size.lat / 2
    }
    if(pt.loc == 4) {
      adj.lon <- -cell.size.lon / 2
      adj.lat <-  cell.size.lat / 2
    }
    if(pt.loc == 5) {
      adj.lon <- cell.size.lon / 2
      adj.lat <- cell.size.lat / 2
    }
    
    # Make new SpatialPixelsDF object
    spdf.pts <- csv.toSPDF
    spdf.pts$Long <- spdf.pts$Long + adj.lon
    spdf.pts$Lat <- spdf.pts$Lat+ adj.lat
    
    coordinates(spdf.pts) <- ~Long+Lat
    proj4string(spdf.pts) <- crs.ll
    spdf.pix <- try(as(spdf.pts, "SpatialPixelsDataFrame"))
    validate(
      need(class(spdf.pix) != "try-error", "Error in adjusting points")
      # Non-lat/long regular points error should have been caught above
    )
  }
  
  # Second check: run dateline function if pixels extend over dateline
  if(extent(spdf.pix)@xmax > 180) {
    print("Data is 0 to 360 deg and spans international dateline; fixing now")
    spdf.poly.ll <- dateline.process(csv.toSPDF, 1, 2)
    # print("Data converted to -180 to 180 deg ")
  } else {   #(extent(spdf.pix)@xmax <= 180) 
    spdf.poly.ll <- as(spdf.pix, "SpatialPolygonsDataFrame")
  }
  
  list(spdf.poly.ll, spdf.pix)
})


### Process data and add it to vals
create_spdf_csv <- eventReactive(input$model_create_csv, {
  withProgress(message = "Loading data from .csv file", value = 0.55, {
    ### Load and process data and input variables    
    lon.idx <- as.numeric(input$model_csv_names_lon)
    lat.idx <- as.numeric(input$model_csv_names_lat)
    pred.idx <- as.numeric(input$model_csv_names_pred)
    error.idx <- as.numeric(input$model_csv_names_error)
    weight.idx <- as.numeric(input$model_csv_names_weight)
    
    error.idx <- ifelse(error.idx == 1, NA, error.idx - 1)
    weight.idx <- ifelse(weight.idx == 1, NA, weight.idx - 1)
    
    csv.idx <- c(lon.idx, lat.idx, pred.idx, error.idx, weight.idx)
    
    csv.all <- read_model_csv()[[2]]
    csv.data <- cbind(csv.all[,csv.idx[1:3]], NA, NA)
    if(!is.na(csv.idx[4])) csv.data[,4] <- csv.all[csv.idx[4]]
    if(!is.na(csv.idx[5])) csv.data[,5] <- csv.all[csv.idx[5]]
    
    # Change invalid densities to NA
    na.idx <- model_csv_NA_idx()
    if(!anyNA(na.idx)) csv.data[na.idx,3:5] <- NA
    
    # Sort data by lat (primary) then long for bottom up sort
    csv.data <- data.sort(csv.data, 2, 1)
    
    # Add 'Pixel' column to keep track of indices
    csv.data <- cbind(csv.data, 1:nrow(csv.data))
    names(csv.data) <- c("Long", "Lat", "Pred", "Error", "Weight", "Pixels")
    
    
    #############################################
    spdf.poly.ll <- create_spdf_csv_grid()[[1]]
    spdf.pix <- create_spdf_csv_grid()[[2]]
    
    spdf.poly.ll@data[,1:3] <- csv.data[,3:5]
    spdf.pix@data[,1:3] <- csv.data[,3:5]
    #############################################
    
    ### Define objects before create_local (common) code
    # spdf.poly.cea <- spTransform(spdf.poly.ll, crs.cea)
    spdf.poly.orig <- spdf.poly.ll
    
    model.name <- read_model_csv()[[1]]
    data.names <- model_csv_names_selected()

    pix.res <- round(unname(spdf.pix@grid@cellsize), 3)
    validate(
      need(pix.res[1] == pix.res[2], 
           "loadModels_csv: Long and Lat pixel width is not the same")
    )
    model.res <- paste(pix.res[1], "degrees")
    
    pred.type <- input$model_csv_pred_type
    
    
    #### Code common to csv, raster, and gis_shp/gis_gdb functions ####
    source(file.path("server_1_loadModels", 
                     "server_1_loadModels_create_local.R"), 
           local = TRUE, echo = FALSE, chdir = TRUE)
    ###################################################################
  })
  
  return("Model predictions loaded from csv")
})