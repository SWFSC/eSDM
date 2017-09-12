### Code for loading a boundary or land polygon from shp or gdb source


############################################################################
# Non-reactive function

### Check that provided SPoly has a valid crs, and return crs.ll version
overlay.gis.crs <- function(gis.loaded) {
  validate(
    need(class(gis.loaded)[1] == "SpatialPolygons", 
         "Object passed to gis.model.check() is not a SPoly")
  )
  
  crs.curr <- crs(gis.loaded)
  validate(
    need(!is.na(crs.curr), "Error: GIS file does not have defined projection")
  )
  
  if(identical(crs.curr, crs.ll)) { 
    gis.loaded
  } else {
    spTransform(gis.loaded, crs.ll)
  }
  
  return(gis.loaded)
}


############################################################################
# Boundary polygon

### Shapefile
overlay_bound_gis_shp <- reactive({
  req(input$overlay_bound_gis_shp_files)
  
  withProgress(message = 'Loading boundary polygon', value = 0.3, {
    bound.gis.shp <- read.shp.in(input$overlay_bound_gis_shp_files)
    validate(
      need(!(class(bound.gis.shp) == "try-error"),
           "Could not load shapefile using selected files") %then%
        need(extent(bound.gis.shp)@xmax <= 180, 
             "Shapefile has longitudes > 180")
    )
    incProgress(0.5)
    
    bound.gis.shp <- as(bound.gis.shp, "SpatialPolygons")
    bound.gis.shp <- overlay.gis.crs(bound.gis.shp)
    incProgress(0.2)
  })
  
  vals$overlay.bound <- bound.gis.shp
  
  return("")
})

# .gdb
overlay_bound_gis_gdb <- eventReactive(input$overlay_bound_gis_gdb_load, { 
  bound.gdb.path <- input$overlay_bound_gis_gdb_path
  bound.gdb.name <- input$overlay_bound_gis_gdb_name
  
  withProgress(message = 'Loading boundary polygon', value = 0.3, {
    bound.gis.gdb <- try(readOGR(bound.gdb.path, bound.gdb.name, 
                                 verbose = FALSE), 
                         silent = TRUE)
    validate(
      need(!(class(bound.gis.gdb) == "try-error"), 
           "Could not load polygon using provided file path and name") %then%
        need(extent(bound.gis.gdb)@xmax <= 180, "File has longitudes > 180")
    )
    incProgress(0.5)
    
    bound.gis.gdb <- as(bound.gis.gdb, "SpatialPolygons")
    bound.gis.gdb <- overlay.gis.crs(bound.gis.gdb)
    incProgress(0.2)
  })  
  
  vals$overlay.bound <- bound.gis.gdb
  
  return("")
})


############################################################################
### Land polygon
# Shapefile
overlay_land_gis_shp <- reactive({
  req(input$overlay_land_gis_shp_files)
  
  withProgress(message = 'Loading land polygon', value = 0.3, {
    land.gis.shp <- read.shp.in(input$overlay_land_gis_shp_files)
    validate(
      need(!(class(land.gis.shp) == "try-error"),
           "Could not load shapefile using selected files") %then%
        need(extent(land.gis.shp)@xmax <= 180, 
             "Shapefile has longitudes > 180")
    )
    incProgress(0.5)
    
    land.gis.shp <- as(land.gis.shp, "SpatialPolygons")
    land.gis.shp <- overlay.gis.crs(land.gis.shp)
    incProgress(0.2)
  })
  
  vals$overlay.land <- land.gis.shp
  
  return("")
})

# .gdb
overlay_land_gis_gdb <- eventReactive(input$overlay_land_gis_gdb_load, { 
  land.gdb.path <- input$overlay_land_gis_gdb_path
  land.gdb.name <- input$overlay_land_gis_gdb_name
  
  withProgress(message = 'Loading land polygon', value = 0.3, {
    land.gis.gdb <- try(readOGR(land.gdb.path, land.gdb.name, 
                                verbose = FALSE), 
                        silent = TRUE)
    validate(
      need(!(class(land.gis.gdb) == "try-error"), 
           "Could not load polygon using provided file path and name") %then%
        need(extent(land.gis.gdb)@xmax <= 180, "File has longitudes > 180")
    )
    incProgress(0.5)
    
    land.gis.gdb <- as(land.gis.gdb, "SpatialPolygons")
    land.gis.gdb <- overlay.gis.crs(land.gis.gdb)
    incProgress(0.2)
  })
  
  vals$overlay.land <- land.gis.gdb
  
  return("")
})