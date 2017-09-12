### Exporting (download) model predictions


###############################################################################
### Unselect rows in other table when a row is selected
# Use 'list()' as NULL causes selectRows to throw a 'deprecated' warning

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
  (length(vals$models.ll) != 0) | (length(vals$ensemble.models) != 0)
})
outputOptions(output, "export_flag", suspendWhenHidden = FALSE)

### Flag for if one of orig, overlaid, and ensemble model preds are selected
output$export_tables_oneselected_flag <- reactive({
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected
  
  sum(!sapply(list(x, y, z), is.null)) == 1
})
outputOptions(output, "export_tables_oneselected_flag", 
              suspendWhenHidden = FALSE)


###############################################################################
# Reactive functions to prep for export

### Return selected predictions 
export_model_selected <- reactive({
  req(vals$models.ll)
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected
  
  req(!sum(sapply(list(x, y, z), is.null)) == 1)
  
  if(!is.null(x)) {
    model.selected <- vals$models.ll[[x]]
  } else if (!is.null(y)) {
    model.selected <- vals$overlaid.models[[y]]
  } else { #!is.null(z)
    model.selected <- vals$ensemble.models[[z]]
  }
  
  model.selected
})

### Return selected predictions in specified crs
export_model_selected_proj <- reactive({
  model.selected <- export_model_selected() # handles req()
  model.selected.crs <- crs(model.selected)
  
  if (input$export_proj == 1) {
    crs.selected <- crs.ll
  } else {
    crs.selected.idx <- as.numeric(input$export_proj) - 1
    crs.selected <- crs(vals$models.orig[[crs.selected.idx]])
  }
  
  if (!identical(model.selected.crs, crs.selected)) {
    model.selected <- spTransform(model.selected, crs.selected)
  }
  
  model.selected
})


### Return selected predictions in specified projection in desired format
export_model_selected_proj_format <- reactive({
  model.selected <- export_model_selected_proj()
  
  if (input$export_format == 1) {
    model.selected.sp <- gCentroid(model.selected, byid = TRUE)
    data.out.coords <- unname(model.selected.sp@coords)
    data.out <- data.frame(Long = data.out.coords[,1], 
                           Lat = data.out.coords[,2], 
                           model.selected@data[,1:3])
  } else { # Exporting data as either .shp and .kml requires SPolyDF
    data.out <- model.selected
  }
  
  data.out
})

###############################################################################
### Export model predictions as csv, shp, or kml
export_out <- eventReactive(input$export_out_execute, {
  withProgress(message = "Exporting predictions", value = 0.4, {
    export.format <- input$export_format
    data.out <- export_model_selected_proj_format()
    
    folder.path <- "C:/Users/sam.woodman/Downloads/Shiny_out"
    filename.all <- paste0(folder.path, "/",input$export_filename)
    
    dir.create(folder.path, showWarnings = FALSE)
    
    if (export.format == 1) {
      write.csv(data.out, file = filename.all, row.names = FALSE)
    }
    if (export.format == 2) {
      writeOGR(data.out, dsn = folder.path, layer = input$export_filename, 
               driver = "ESRI Shapefile")
    }
    if (export.format == 3) {
      writeOGR(data.out, dsn = filename.all, layer = input$export_filename, 
               driver = "KML")
    }
    
    incProgress(0.6)
    Sys.sleep(0.5)
  })
  
  return()
})

###############################################################################
