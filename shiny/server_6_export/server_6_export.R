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

### Flag for if filename extension is correct
output$export_filename_flag <- reactive({
  export.format <- input$export_format
  ext.text <- switch(as.numeric(input$export_format), 
                     ".csv", ".shp", 
                     ifelse(input$export_format_kml == 1, ".kml", ".kmz"))
  
  substrRight(input$export_filename, 4) == ext.text
})
outputOptions(output, "export_filename_flag", suspendWhenHidden = FALSE)


###############################################################################
# Reactive functions that perform export prep steps

### Get selected predictions 
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
  } else if (!is.null(z)) {
    model.selected <- vals$ensemble.models[[z]]
  } else {
    validate(need(FALSE, "Error: export get predictions error"))
  }
  
  ### Retiurn Pred and Weight data
  to.return <- model.selected[c(1, 3)]
  names(to.return) <- c("Density", "Weight")
  
  to.return
})

### Return selected predictions in specified crs
export_model_selected_proj <- reactive({
  model.selected <- export_model_selected() # handles req()
  model.selected.crs <- crs(model.selected)
  
  if (input$export_proj_ll) {
    crs.selected <- crs.ll
  } else {
    crs.selected.idx <- as.numeric(input$export_proj)
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
    model.selected.centroid <- gCentroid(model.selected, byid = TRUE)
    data.out.coords <- unname(model.selected.centroid@coords)
    data.out <- data.frame(Long = data.out.coords[, 1], 
                           Lat = data.out.coords[, 2], 
                           model.selected@data)
  } else { # Exporting data as either .shp and .kml requires SPolyDF
    data.out <- model.selected
  }
  
  data.out
})

###############################################################################
### Export model predictions as csv, shp, or kml
# Download shapefile code is modeled after 
# https://gist.github.com/RCura/b6b1759ddb8ab4035f30
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
      export.format <- input$export_format
      data.out <- export_model_selected_proj_format()
      
      # browser()
      if (export.format == 1) {
        write.csv(data.out, file = file, row.names = FALSE)
        
      } else if (export.format == 2) {
        # browser()
        # name.base <- paste0(tempdir(), "\\", "shpExport")
        name.base <- gsub(".{4}$", "", input$export_filename)
        name.glob <- paste0(name.base, ".*")
        name.shp <- paste0(name.base, ".shp")
        name.zip <- paste0(name.base, ".zip")
        print(name.base)
        
        if (length(Sys.glob(name.glob)) > 0) {
          file.remove(Sys.glob(name.glob))
        }
        writeOGR(data.out, dsn = name.shp, layer = "shpExport", driver = "ESRI Shapefile")
        zip(zipfile = name.zip, files = Sys.glob(name.glob))
        file.copy(name.zip, file)

        if (length(Sys.glob(name.glob)) > 0) {
          file.remove(Sys.glob(name.glob))
        }
        
      } else if (export.format == 3) {
        writeOGR(data.out, dsn = file, layer = "Pred_kml", driver = "KML")
        
      } else {
        validate(need(FALSE, "Download error"))
      }
      
      incProgress(0.6)
    })
  }
)



# output$downloadShp <- downloadHandler(
#   filename = "shpExport.zip",
# 
#   content = function(file) {
#     if (length(Sys.glob("shpExport.*")) > 0) {
#       file.remove(Sys.glob("shpExport.*"))
#     }
#     writeOGR(createShp(), dsn = "shpExport.shp", layer = "shpExport", driver = "ESRI Shapefile")
#     zip(zipfile = 'shpExport.zip', files = Sys.glob("shpExport.*"))
#     file.copy("shpExport.zip", file)
#     
#     if (length(Sys.glob("shpExport.*")) > 0) {
#       file.remove(Sys.glob("shpExport.*"))
#     }
#   }
# )


###############################################################################
