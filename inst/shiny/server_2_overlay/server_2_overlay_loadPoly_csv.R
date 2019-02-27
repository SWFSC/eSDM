### Code for creating a study area or erasing polygon from .csv data


############################################################################
# Study area polygon (aka boundary in code)

### Create sfc object with one polygon from csv points
overlay_bound_csv <- reactive({
  req(input$overlay_bound_csv_file)

  # Reset vals object here in case validate() is triggered
  vals$overlay.bound <- NULL

  # Read in .csv file
  validate(
    need(input$overlay_bound_csv_file$type %in%
           c("text/csv", "application/vnd.ms-excel"),
         "Error: Selected file is not an Excel .csv file")
  )
  csv.df <- read.csv(
    input$overlay_bound_csv_file$datapath, stringsAsFactors = FALSE
  )

  validate(
    need(ncol(csv.df) >= 2,
         paste("Error: The study area .csv file must have at least two",
               "columns (longitude and latitude, respectively)")),
    need(!anyNA(csv.df),
         paste("Error: The study area polygon must be one, single polygon.",
               "Please load a csv file without any invalid entries (e.g. NA)",
               "in the two columns"))
  )

  # Process input and make it into sfc object
  withProgress(message = 'Importing study area polygon', value = 0.7, {
    Sys.sleep(0.5)

    if (min(csv.df[, 1]) > 180) csv.df[, 1] <- csv.df[, 1] - 360
    bound.sfc <- try(
      st_sfc(st_polygon(list(as.matrix(csv.df))), crs = 4326), silent = TRUE
    )
    validate(
      need(isTruthy(bound.sfc),
           paste("Error: The study area polygon could not be created",
                 "from the provided points.",
                 "Please ensure that the .csv file has the longitude points",
                 "in the first column, the latitude points in the second",
                 "column, and that the provided points form a closed",
                 "and valid polygon")) %then%
        need(st_is_valid(bound.sfc),
             paste("Error: The provided study area polygon is invalid;",
                   "please ensure that the provided points form a closed",
                   "and valid polygon (no self-intersections)"))
    )

    bound.sfc <- check_dateline(bound.sfc, 60, progress.detail = TRUE)
    incProgress(0.3)
  })

  # Add sfc object to vals, this sfc object will always be length 1
  vals$overlay.bound <- bound.sfc

  # Return empty string - success message printed elsewhere
  ""
})


############################################################################
# Erasing polygon (aka land in code)

### Create sfc object from csv points
overlay_land_csv <- reactive({
  req(input$overlay_land_csv_file)

  # Reset vals object here in case validate() is triggered
  vals$overlay.land <- NULL

  # Read in .csv file
  validate(
    need(input$overlay_land_csv_file$type %in%
           c("text/csv", "application/vnd.ms-excel"),
         "Error: Selected file is not an Excel .csv file")
  )
  csv.df <- read.csv(
    input$overlay_land_csv_file$datapath, stringsAsFactors = FALSE
  )

  # Create sfc object for land polygon
  withProgress(message = 'Importing land polygon', value = 0.7, {
    Sys.sleep(0.5)

    if (min(csv.df[, 1], na.rm = TRUE) > 180) {
      csv.df[, 1] <- csv.df[, 1] - 360
    }
    land.sfc <- pts2poly_vertices_shiny(csv.df[, 1:2], crs.ll, TRUE)
    #^ Calls check_dateline() and check_valid()
    incProgress(0.3)
  })

  # Add sfc object to vals
  vals$overlay.land <- land.sfc

  # Return empty string - success message printed elsewhere
  ""
})

###############################################################################
