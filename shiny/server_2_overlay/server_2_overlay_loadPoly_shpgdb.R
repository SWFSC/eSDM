### Code for loading a boundary or land polygon from shp or gdb source
## overlaid.gis.crs is in ...overlay_funcs.R

###############################################################################
# Boundary polygon

### Shapefile
overlay_bound_gis_shp <- reactive({
  req(input$overlay_bound_gis_shp_files)
  
  withProgress(message = 'Loading boundary polygon', value = 0.3, {
    bound.gis.shp <- read.shp.in(input$overlay_bound_gis_shp_files)
    
    validate(
      need(isTruthy(bound.gis.shp),
           "Error: Could not load shapefile using selected files") %then%
        need(extent(bound.gis.shp)@xmax <= 180 & 
               extent(bound.gis.shp)@xmin >= -180, 
             "Error: Shapefile has longitudes > 180 or < -180")
    )
    incProgress(0.5)
    
    bound.gis.shp <- as(bound.gis.shp, "SpatialPolygons")
    bound.gis.shp <- overlay.gis.crs(bound.gis.shp)
    incProgress(0.2)
  })
  
  vals$overlay.bound <- bound.gis.shp
  
  return("")
})

### .gdb
overlay_bound_gis_gdb <- eventReactive(input$overlay_bound_gis_gdb_load, { 
  bound.gdb.path <- input$overlay_bound_gis_gdb_path
  bound.gdb.name <- input$overlay_bound_gis_gdb_name
  
  withProgress(message = 'Loading boundary polygon', value = 0.3, {
    bound.gis.gdb <- try(readOGR(bound.gdb.path, bound.gdb.name, 
                                 verbose = FALSE), 
                         silent = TRUE)
    
    validate(
      need(isTruthy(bound.gis.gdb), 
           paste("Error: Could not load polygon using", 
                 "provided file path and name")) %then%
        need(extent(bound.gis.gdb)@xmax <= 180 & 
               extent(bound.gis.gdb)@xmin >= -180, 
             "Error: File has longitudes > 180 or < -180")
    )
    incProgress(0.5)
    
    bound.gis.gdb <- as(bound.gis.gdb, "SpatialPolygons")
    bound.gis.gdb <- overlay.gis.crs(bound.gis.gdb)
    incProgress(0.2)
  })  
  
  vals$overlay.bound <- bound.gis.gdb
  
  return("")
})


###############################################################################
# Land polygon

### Shapefile
overlay_land_gis_shp <- reactive({
  req(input$overlay_land_gis_shp_files)
  
  withProgress(message = 'Loading land polygon', value = 0.3, {
    land.gis.shp <- read.shp.in(input$overlay_land_gis_shp_files)
    
    need(isTruthy(land.gis.shp),
         "Error: Could not load shapefile using selected files") %then%
      need(extent(land.gis.shp)@xmax <= 180 & 
             extent(land.gis.shp)@xmin >= -180, 
           "Error: Shapefile has longitudes > 180 or < -180")
    incProgress(0.5)
    
    land.gis.shp <- as(land.gis.shp, "SpatialPolygons")
    land.gis.shp <- overlay.gis.crs(land.gis.shp)
    incProgress(0.2)
  })
  
  vals$overlay.land <- land.gis.shp
  
  return("")
})

### .gdb
overlay_land_gis_gdb <- eventReactive(input$overlay_land_gis_gdb_load, { 
  land.gdb.path <- input$overlay_land_gis_gdb_path
  land.gdb.name <- input$overlay_land_gis_gdb_name
  
  withProgress(message = 'Loading land polygon', value = 0.3, {
    land.gis.gdb <- try(readOGR(land.gdb.path, land.gdb.name, 
                                verbose = FALSE), 
                        silent = TRUE)
    validate(
      need(isTruthy(land.gis.gdb), 
           paste("Error: Could not load polygon using", 
                 "provided file path and name")) %then%
        need(extent(land.gis.gdb)@xmax <= 180 & 
               extent(land.gis.gdb)@xmin >= -180, 
             "Error: File has longitudes > 180 or < -180")
    )
    incProgress(0.5)
    
    land.gis.gdb <- as(land.gis.gdb, "SpatialPolygons")
    land.gis.gdb <- overlay.gis.crs(land.gis.gdb)
    incProgress(0.2)
  })
  
  vals$overlay.land <- land.gis.gdb
  
  return("")
})

###############################################################################
