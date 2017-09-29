### Code for creating a boundary or land polygon from loaded csv data


############################################################################
# Boundary polygon

### Make sure file is.csv and read it in
overlay_bound_read_csv <- reactive({
  file.in <- input$overlay_bound_csv_file
  req(file.in)
  
  # Ensure file ext is .csv; use validate since there are no renderUI's
  validate(
    need(file.in$type %in% c("text/csv", "application/vnd.ms-excel"), 
         "Error: Selected file is not a csv file")
  )
  
  csv.df <- read.csv(file.in$datapath)[, 1:2]
  validate(
    need(!anyNA(csv.df), 
         paste("Error: Please load a csv file without invalid entries", 
               "in the longitude and latitude columns"))
  )
  
  return(csv.df)
})

### Create SpatialPolygons with one polygon from csv points
overlay_bound_csv <- reactive({
  req(input$overlay_bound_csv_file)
  
  withProgress(message = 'Loading boundary polygon', value = 0.7, {
    csv.df <- overlay_bound_read_csv()
    Sys.sleep(0.5)
    
    bound.csv <- SpatialPolygons(list(
      Polygons(list(Polygon(cbind(csv.df[,1], csv.df[,2]))), ID = "bound")))
    crs(bound.csv) <- crs.ll
    
    incProgress(0.3)
  })
  
  vals$overlay.bound <- bound.csv
  
  return("")
})


############################################################################
# Land polygon

### Make sure file is.csv and read it in
overlay_land_read_csv <- reactive({
  file.in <- input$overlay_land_csv_file
  req(file.in)
  
  # Ensure file ext is .csv; use validate since there are no renderUI's
  validate(
    need(file.in$type %in% c("text/csv", "application/vnd.ms-excel"), 
         "Error: Selected file is not a csv file")
  )
  
  csv.df <- read.csv(file.in$datapath)[, 1:2]
  csv.df[,1] <- ifelse(csv.df[,1] > 180, csv.df[,1] - 360, csv.df[,1])
  validate(
    need(all(csv.df[, 1] <= 180 & csv.df[, 1] >= -180),
         "Error: .csv file has longitudes > 180")
  )
  
  return(csv.df)
})

### Create SpatialPolygons from csv points
overlay_land_csv <- reactive({
  req(input$overlay_land_csv_file)
  
  withProgress(message = 'Loading land polygon', value = 0.7, {
    csv.df <- overlay_land_read_csv()
    Sys.sleep(0.5)
    
    csv.df.na <- c(0, which(is.na(csv.df$lon) & is.na(csv.df$lat)), 
                   nrow(csv.df) + 1)
    
    csv.df.nona <- mapply(function(i, j) {
      cbind(csv.df[(i+1):(j-1),], (i+1):(j-1)) 
    }, 
    csv.df.na[1:16], csv.df.na[2:17], SIMPLIFY = FALSE)
    csv.df.nona <- csv.df.nona[sapply(csv.df.nona, function(i) nrow(i) >= 4)]
    
    land.csv <- disaggregate(SpatialPolygons(list(
      Polygons(
        lapply(csv.df.nona, function(i) {
          Polygon(cbind(i[,1], i[,2]), hole = FALSE)
        }), 
        ID = "land"))))
    crs(land.csv) <- crs.ll
    
    incProgress(0.3)
  })
  
  vals$overlay.land <- land.csv
  
  return("")
})

###############################################################################
