### Code for creating a boundary or land polygon from loaded csv data


############################################################################
# Boundary polygon

### Create sfc object with one polygon from csv points
overlay_bound_csv <- reactive({
  req(input$overlay_bound_csv_file)

  # Reset vals object here in case validate() is triggered
  vals$overlay.bound <- NULL

  # Read in .csv file
  validate(
    need(input$overlay_bound_csv_file$type %in%
           c("text/csv", "application/vnd.ms-excel"),
         "Error: Selected file is not a csv file")
  )
  csv.df <- read.csv(input$overlay_bound_csv_file$datapath,
                     stringsAsFactors = FALSE)

  # This reactive func does not go to renderUI, so validate is ok to use
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

    if (st_bbox(bound.sfc)[3] > 180) {
      incProgress(detail = "Polygon spans dateline; handling now")
      bound.sfc <- st_wrap_dateline(
        bound.sfc, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=60")
      )
    }

    incProgress(0.3)
  })

  # Add sfc object to vals, this sfc object will always be length 1
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

  # Read in .csv file
  validate(
    need(input$overlay_land_csv_file$type %in%
           c("text/csv", "application/vnd.ms-excel"),
         "Error: Selected file is not a csv file")
  )
  csv.df <- read.csv(input$overlay_land_csv_file$datapath,
                     stringsAsFactors = FALSE)

  # Create sfc object for land polygon
  withProgress(message = 'Loading land polygon', value = 0.7, {
    Sys.sleep(0.5)

    land.sfc <- create_sfc_csv(csv.df, crs.ll)
    incProgress(0.3)
  })

  # Add sfc object to vals
  vals$overlay.land <- land.sfc

  # Return empty string - success message printed elsewhere
  ""
})

###############################################################################
