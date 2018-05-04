### Code for creating a boundary or land polygon from loaded csv data


############################################################################
# Boundary polygon

### Create sfc object with one polygon from csv points
overlay_bound_csv <- reactive({
  req(input$overlay_bound_csv_file)
  
  # Reset vals object here in case validate() is triggered
  vals$overlay.bound <- NULL
  
  # Read in .csv file, convert longs to [-180, 180] if all are greater 
  # than 180, and perform checks. Only need df (second object in returned list)
  csv.df <- read.csv.shiny(input$overlay_bound_csv_file)[[2]][, 1:2]
  
  # This reactive func does not go to renderUI, so validate is good to use
  # TODO should boundary csv file only be allowed one polygon?
  validate(
    need(!anyNA(csv.df), 
         paste("Error: Boundary polygon must be one, single polygon.", 
               "Please load a csv file without invalid entries (such as NA)", 
               "in the longitude and latitude columns"))
  )
  
  # Process input and make it into sf object
  withProgress(message = 'Loading boundary polygon', value = 0.7, {
    Sys.sleep(0.5)
    
    bound.sfc <- try(st_sfc(st_polygon(list(as.matrix(csv.df)))), 
                     silent = TRUE)
    validate(
      need(isTruthy(bound.sfc), 
           paste("Error: The boundary polygon could not be created", 
                 "from the provided points.", 
                 "Please ensure that the .csv file has the longitude points", 
                 "in the first column, the latitude points in the second", 
                 "column, and that the provided points form a closed", 
                 "and valid polygon")) %then%
        need(st_is_valid(bound.sfc), 
             paste("Error: The provided boundary polygon is invalid;", 
                   "please ensure that the provided points form a closed", 
                   "and valid polygon (no self-intersections)"))
    )
    st_crs(bound.sfc) <- crs.ll
    
    incProgress(0.3)
  })
  
  # Add sf object to vals
  vals$overlay.bound <- bound.sfc
  
  # Return empty string - success message printed elsewhere
  ""
})


############################################################################
# Land polygon

### Create sfc object from csv points
overlay_land_csv <- reactive({
  req(input$overlay_land_csv_file)
  
  # Reset vals object here in case validate() is triggered
  vals$overlay.land <- NULL
  
  # Read in .csv file, convert longs to [-180, 180] if they're all greater 
  # than 180, and perform checks. Only need df (second object in returned list)
  csv.df <- read.csv.shiny(input$overlay_land_csv_file)[[2]][, 1:2]
  
  # Create sfc object for land polygon
  withProgress(message = 'Loading land polygon', value = 0.7, {
    Sys.sleep(0.5)
    
    names(csv.df) <- c("lon", "lat")
    land.list <- try(
      csv.df %>% 
        mutate(na_sum = cumsum(is.na(lon) & is.na(lat))) %>% 
        filter(!is.na(lon) & !is.na(lat)) %>%
        group_by(na_sum) %>% 
        summarise(list(st_polygon(list(matrix(c(.data$lon, .data$lat), ncol = 2))))), 
      silent = TRUE)
    
    land.sfc <- try(st_sfc(do.call(rbind, land.list[, 2])), silent = TRUE)
    
    validate(
      need(isTruthy(land.list) & 
             identical(class(land.sfc), c("sfc_POLYGON", "sfc")), 
           paste("Error: The boundary polygon could not be created", 
                 "from the provided points.", 
                 "Please ensure that the .csv file has the longitude points", 
                 "in the first column, the latitude points in the second", 
                 "column, and that the provided points form a closed", 
                 "and valid polygon")) %then%
        need(isTruthy(all(st_is_valid(land.sfc))), #isTruthy() is for NA cases
             paste("Error: The provided boundary polygon is invalid;", 
                   "please ensure that the provided points form a closed", 
                   "and valid polygon (no self-intersections)"))
    )
    st_crs(land.sfc) <- crs.ll
    
    incProgress(0.3)
  })
  
  # Add sfc object to vals
  vals$overlay.land <- land.sfc
  
  # Return empty string - success message printed elsewhere
  ""
})

###############################################################################
