### Exporting (download) model predictions


###############################################################################
### Unselect rows in other table when a row is selected
# Use 'list()' because 'NULL' causes selectRows to throw a 'deprecated' warning

observeEvent(input$export_table_orig_out_rows_selected, {
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected

  if(!is.null(x) & !is.null(y)) {
    dataTableProxy("export_table_over_out") %>% selectRows(list())
  }
  if(!is.null(x) & !is.null(z)) {
    dataTableProxy("export_table_ens_out") %>% selectRows(list())
  }
}, ignoreInit = TRUE)

observeEvent(input$export_table_over_out_rows_selected, {
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected

  if(!is.null(y) & !is.null(x)) {
    dataTableProxy("export_table_orig_out") %>% selectRows(list())
  }
  if(!is.null(y) & !is.null(z)) {
    dataTableProxy("export_table_ens_out") %>% selectRows(list())
  }
}, ignoreInit = TRUE)

observeEvent(input$export_table_ens_out_rows_selected, {
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected

  if(!is.null(z) & !is.null(x)) {
    dataTableProxy("export_table_orig_out") %>% selectRows(list())
  }
  if(!is.null(z) & !is.null(y)) {
    dataTableProxy("export_table_over_out") %>% selectRows(list())
  }
}, ignoreInit = TRUE)


###############################################################################
# Flags

### Flag for if any models are loaded
output$export_flag <- reactive({
  list.models.all <- list(
    vals$models.ll, vals$overlaid.models, vals$ensemble.models
  )
  any(sapply(list.models.all, length) > 0)
})
outputOptions(output, "export_flag", suspendWhenHidden = FALSE)

### Flag for if one of orig, overlaid, and ensemble model preds are selected
output$export_tables_oneselected_flag <- reactive({
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected

  # Use >= 1 here so that error message doesn't pop up briefly when
  #   user selects a model from a different table
  sum(!sapply(list(x, y, z), is.null)) >= 1
})
outputOptions(output, "export_tables_oneselected_flag",
              suspendWhenHidden = FALSE)

### Flag for if sdm will be exported in non-long/lat crs
export_nonll <- reactive({
  req(export_crs()[[2]])
  input$export_format == 1 && !st_is_longlat(export_crs())
})
output$export_nonll_flag <- reactive(export_nonll())
outputOptions(output, "export_nonll_flag", suspendWhenHidden = FALSE)

### Flag for if selected predictions span dateline
output$export_range360_flag <- reactive({
  check_360(export_model_selected())
})
outputOptions(output, "export_range360_flag", suspendWhenHidden = FALSE)


###############################################################################
### Export model predictions as csv, shp, or kml
# Download shapefile code is adapted from
#   https://gist.github.com/RCura/b6b1759ddb8ab4035f30
output$export_out <- downloadHandler(
  filename = function() {
    if (input$export_format == 2) {
      "eSDM_shpExport.zip"
    } else {
      input$export_filename
    }
  },

  content = function(file) {
    withProgress(message = "Exporting predictions", value = 0.4, {
      # Prep
      export.format <- input$export_format
      data.out <- export_model_selected_proj_format()
      incProgress(0.3)

      # Write file
      if (export.format == 1) {
        write.csv(data.out, file = file, row.names = FALSE)

      } else if (export.format == 2) {
        name.base <- gsub(".{4}$", "", input$export_filename)
        name.glob <- paste0(name.base, ".*")
        name.shp <- paste0(name.base, ".shp")
        name.zip <- paste0(name.base, ".zip")

        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        st_write(data.out, dsn = name.shp, layer = "shpExport",
                 driver = "ESRI Shapefile", quiet = TRUE)
        zip(zipfile = name.zip, files = Sys.glob(name.glob))
        file.copy(name.zip, file)

        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))

      } else if (export.format == 3) {
        st_write(data.out, dsn = file, layer = "Pred_kml", driver = "KML",
                 quiet = TRUE)

      } else {
        validate(need(FALSE, "Download error"))
      }

      incProgress(0.3)
    })
  }
)


###############################################################################
# Reactive functions that perform export prep steps

### Get selected predictions
export_model_selected <- reactive({
  req(length(vals$models.orig) > 0)

  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected
  req(sum(!sapply(list(x, y, z), is.null)) == 1)

  if (!is.null(x)) {
    model.selected <- vals$models.orig[[x]]

  } else if (!is.null(y)) {
    model.selected <- vals$overlaid.models[[y]]

  } else { #!is.null(z)
    model.selected <- vals$ensemble.models[[z]]
  }

  # Pred column (column 1) is only column all 3 model types have in common
  st_sf(
    dplyr::select(st_set_geometry(model.selected, NULL), Density = 1),
    st_geometry(model.selected), agr = "constant"
  )
})


### Get crs in which to export model predictions
# No validate() or req() in this function so nothing is passed to export_csv_ll
export_crs <- reactive({
  if (input$export_proj_native) {
    crs.selected <- st_crs(export_model_selected()) #handles req()

  } else {
    crs.selected <- switch(
      as.numeric(input$export_proj_method),
      crs.ll,
      st_crs(vals$models.orig[[as.numeric(req(input$export_proj_sdm))]]),
      suppressWarnings(st_crs(input$export_proj_epsg))
    )
  }

  validate(
    need(crs.selected[[2]],
         paste("Error: The provided coordinate system was not recognized,",
               "please specify a valid coordinate system"))
  )

  crs.selected
})


### Return selected predictions in specified crs
export_model_selected_proj <- reactive({
  model.selected <- export_model_selected() #handles req()
  crs.selected <- export_crs()

  if (!identical(st_crs(model.selected), crs.selected)) {
    st_transform(model.selected, crs.selected)
  } else {
    model.selected
  }
})


### Return selected predictions in specified projection in desired format
export_model_selected_proj_format <- reactive({
  model.selected <- export_model_selected_proj() #handles req()

  # Transform longitudes to equiv of range [0, 360] if necessary
  if (input$export_proj_360) {
    if (check_360(model.selected)) {
      incProgress(0, detail = "Processing predictions that span dateline")
      model.selected <- preview360_mod(model.selected)
      incProgress(0, detail = "")

    } else {
      range.poly <- st_transform(
        pretty_range_poly_func(c(-180, -90, 0, 90), 4326),
        st_crs(model.selected)
      )
      cover.out <- suppressMessages(st_covers(range.poly, model.selected)[[1]])
      if (length(cover.out) > 0) {
        incProgress(0, detail = "Processing predictions")
        model.selected <- preview360_mod(model.selected)
        incProgress(0, detail = "")
      }
    }
  }

  # Exporting data as .csv requires df of centroids; otherwise return sf obj
  if (input$export_format == 1) {
    # Must be after transformation so centroids are accurate
    model.selected.centroid <- suppressWarnings(suppressMessages(
      st_centroid(model.selected)
    ))
    data.out.coords <- do.call(rbind, st_geometry(model.selected.centroid))

    data.out <- data.frame(
      Long = data.out.coords[, 1], Lat = data.out.coords[, 2],
      st_set_geometry(model.selected, NULL)
    )

    # # Add centroid coordinates in crs.ll if desired
    # if (export_nonll() && input$export_csv_ll) {
    #   x.cent.ll <- st_transform(model.selected.centroid, 4326)
    #   if (input$export_proj_360) x.cent.ll <- preview360_split(x.cent.ll)
    #   data.out.coords.ll <- do.call(rbind, st_geometry(x.cent.ll))
    #
    #   data.out <- cbind(data.out, Long_ll = data.out.coords.ll[, 1],
    #                     Lat_ll = data.out.coords.ll[, 2])
    # }

    data.out

  } else {
    model.selected
  }
})

###############################################################################
