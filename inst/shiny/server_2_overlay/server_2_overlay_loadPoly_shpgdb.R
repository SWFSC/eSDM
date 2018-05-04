### Code for loading a boundary or land polygon from shp or gdb source
## overlaid.gis.crs is in ...overlay_funcs.R

###############################################################################
# Study area/boundary polygon

### Shapefile
overlay_bound_gis_shp <- reactive({
  req(input$overlay_bound_gis_shp_files)

  # Reset vals object here in case validate() is triggered
  vals$overlay.bound <- NULL

  withProgress(message = 'Loading boundary polygon', value = 0.3, {
    bound.sf <- eSDM::shiny.read.shp(input$overlay_bound_gis_shp_files)

    validate(
      need(identical(class(bound.sf), c("data.frame", "sf")),
           "Error: Could not load shapefile using selected files")
    )
    incProgress(0.5)

    vals$overlay.bound <- overlay.gis.crs(st_geometry(bound.sf))
    incProgress(0.2)
  })

  ""
})


### .gdb
overlay_bound_gis_gdb <- eventReactive(input$overlay_bound_gis_gdb_load, {
  # Reset vals object here in case validate() is triggered
  vals$overlay.bound <- NULL

  withProgress(message = 'Loading boundary polygon', value = 0.3, {
    bound.sf <- try(st_read(input$overlay_bound_gis_gdb_path,
                            input$overlay_bound_gis_gdb_name,
                            quiet = TRUE),
                    silent = TRUE)

    validate(
      need(identical(class(bound.sf), c("data.frame", "sf")),
           "Error: Could not load object using provided path and name")
    )
    incProgress(0.5)

    vals$overlay.bound <- overlay.gis.crs(st_geometry(bound.sf))
    incProgress(0.2)
  })

  ""
})


###############################################################################
# Land polygon

### Shapefile
overlay_land_gis_shp <- reactive({
  req(input$overlay_land_gis_shp_files)

  # Reset vals object here in case validate() is triggered
  vals$overlay.land <- NULL

  withProgress(message = 'Loading land polygon', value = 0.3, {
    land.sf <- eSDM::shiny.read.shp(input$overlay_land_gis_shp_files)

    validate(
      need(identical(class(land.sf), c("data.frame", "sf")),
           "Error: Could not load shapefile using selected files")
    )
    incProgress(0.5)

    vals$overlay.land <- overlay.gis.crs(st_geometry(land.sf))
    incProgress(0.2)
  })

  ""
})


### .gdb
overlay_land_gis_gdb <- eventReactive(input$overlay_land_gis_gdb_load, {
  land.gdb.path <- input$overlay_land_gis_gdb_path
  land.gdb.name <- input$overlay_land_gis_gdb_name

  # Reset vals object here in case validate() is triggered
  vals$overlay.land <- NULL

  withProgress(message = 'Loading land polygon', value = 0.3, {
    land.sf <- try(st_read(input$overlay_land_gis_gdb_path,
                            input$overlay_land_gis_gdb_name,
                            quiet = TRUE),
                    silent = TRUE)

    validate(
      need(identical(class(land.sf), c("data.frame", "sf")),
           "Error: Could not load object using provided path and name")
    )
    incProgress(0.5)

    vals$overlay.land <- overlay.gis.crs(st_geometry(land.sf))
    incProgress(0.2)
  })

  ""
})

###############################################################################
