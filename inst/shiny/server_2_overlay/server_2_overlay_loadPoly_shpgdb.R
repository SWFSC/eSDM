### Code for importing a study area or erasing polygon from shp or gdb source
## overlaid.gis.crs is in ...overlay_funcs.R

###############################################################################
# Study area (aka boundary) polygon

### Shapefile
overlay_bound_gis_shp <- reactive({
  req(input$overlay_bound_gis_shp_files)

  isolate({
    # Reset vals object here in case validate() is triggered
    vals$overlay.bound <- NULL

    withProgress(message = 'Importing study area polygon', value = 0.3, {
      bound.sf <- read.shp.shiny(input$overlay_bound_gis_shp_files)

      validate(
        need(inherits(bound.sf, c("sf", "sfc")),
             "Error: Could not import shapefile using selected files")
      )
      incProgress(0.5)

      temp.bound <- overlay_gis_check(st_geometry(bound.sf))

      if (length(temp.bound) != 1) {
        vals$overlay.bound <- st_union(temp.bound)
      } else {
        vals$overlay.bound <- temp.bound
      }

      incProgress(0.2)
    })
  })

  ""
})


### .gdb
overlay_bound_gis_gdb <- eventReactive(input$overlay_bound_gis_gdb_load, {
  # Reset vals object here in case validate() is triggered
  vals$overlay.bound <- NULL

  withProgress(message = 'Importing study area polygon', value = 0.3, {
    bound.sf <- try(
      st_read(input$overlay_bound_gis_gdb_path,
              input$overlay_bound_gis_gdb_name,
              quiet = TRUE),
      silent = TRUE
    )

    validate(
      need(inherits(bound.sf, c("sf", "sfc")),
           "Error: Could not import object using provided path and name")
    )
    incProgress(0.5)

    temp.bound <- overlay_gis_check(st_geometry(bound.sf))

    if (length(temp.bound) != 1) {
      vals$overlay.bound <- st_union(temp.bound)
    } else {
      vals$overlay.bound <- temp.bound
    }
    incProgress(0.2)
  })

  ""
})


###############################################################################
# Erasing (aka land) polygon

### Shapefile
overlay_land_gis_shp <- reactive({
  req(input$overlay_land_gis_shp_files)

  isolate({
    # Reset vals object here in case validate() is triggered
    vals$overlay.land <- NULL

    withProgress(message = 'Importing erasing polygon', value = 0.3, {
      land.sf <- read.shp.shiny(input$overlay_land_gis_shp_files)

      validate(
        need(inherits(land.sf, c("sf", "sfc")),
             "Error: Could not import shapefile using selected files")
      )
      incProgress(0.5)

      temp.land <- overlay_gis_check(st_geometry(land.sf))

      if (length(temp.land) != 1) {
        vals$overlay.land <- st_union(temp.land)
      } else {
        vals$overlay.land <- temp.land
      }
      incProgress(0.2)
    })
  })

  ""
})


### .gdb
overlay_land_gis_gdb <- eventReactive(input$overlay_land_gis_gdb_load, {
  land.gdb.path <- input$overlay_land_gis_gdb_path
  land.gdb.name <- input$overlay_land_gis_gdb_name

  # Reset vals object here in case validate() is triggered
  vals$overlay.land <- NULL

  withProgress(message = 'Importing erasing polygon', value = 0.3, {
    land.sf <- try(
      st_read(input$overlay_land_gis_gdb_path, input$overlay_land_gis_gdb_name,
              quiet = TRUE),
      silent = TRUE
    )

    validate(
      need(inherits(land.sf, c("sf", "sfc")),
           "Error: Could not import object using provided path and name")
    )
    incProgress(0.5)

    temp.land <- overlay_gis_check(st_geometry(land.sf))

    if (length(temp.land) != 1) {
      vals$overlay.land <- st_union(temp.land)
    } else {
      vals$overlay.land <- temp.land
    }
    incProgress(0.2)
  })

  ""
})

###############################################################################
