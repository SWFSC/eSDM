### renderUI code for Export Model Predictions tab


###############################################################################
### Model with coord system in which to export selected model predictions
output$export_proj_sdm_uiOut_select <- renderUI({
  req(vals$models.names, !input$export_proj_native,
      input$export_proj_method == 2)

  choices.list.names <- vals$models.names
  choices.list <- seq_along(choices.list.names)
  names(choices.list) <- choices.list.names

  selectInput("export_proj_sdm",
              tags$h5("Export predictions in the coordinate system of the",
                      "selected SDM predictions"),
              choices = choices.list, selected = 1)
})


###############################################################################
### Default filename of exported object
output$export_filename_uiOut_text <- renderUI({
  req(length(vals$models.ll) > 0)
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected
  req(sum(!sapply(list(x, y, z), is.null)) == 1)

  #------------------------------------
  ### Extract first term of filename
  if (isTruthy(x)) {
    # Orig model predictions
    table.info <- table_orig()[x, ]
    filename.value <- paste(table.info[, 1:2], collapse = "__")
    filename.value <- paste0(filename.value, "__orig")

  } else if (isTruthy(y)) {
    # Overlaid model predictions
    table.info <- table_overlaid()[y, ]
    filename.value <- paste(table.info[, 1:2], collapse = "__")
    filename.value <- paste0(filename.value, "__overlaid")

  } else {  # isTruthy(z)
    # Ensemble model predictions
    table.info <- table_ensembles()[z, ]

    table.info$`Ensembling method` <- switch(
      table.info$`Ensembling method`, "Unweighted" = "UnW", "Weighted" = "W"
    )

    rescale.txt <- table.info$`Rescaling method`
    rescale.txt <- ifelse(
      grepl("Abund", rescale.txt),
      paste0("Abund", strsplit(rescale.txt, ": ")[[1]][2]),
      switch(rescale.txt,
             "None" = "None", "Normalization" = "Norm",
             "Standardization" = "Stand", "Sum to 1" = "Sumto1_")
    )
    table.info$`Rescaling method` <- rescale.txt

    filename.value <- paste(table.info[, c(1, 3:4)], collapse = "_")
    filename.value <- gsub(", ", "+", filename.value) # Handles columns 2 & 4
  }

  #------------------------------------
  ### Projection info
  if (input$export_proj_360) filename.value <- paste0(filename.value, "_360")

  #------------------------------------
  ### Prefix and extensions
  filename.ext <- switch(
    input$export_format, "1" = ".csv", "2" = ".shp",
    "3" = ifelse(input$export_format_kml == 1, ".kml", ".kmz")
  )
  filename.value <- gsub("\\.", "_", filename.value)
  filename.value <- paste0("eSDM_", filename.value, filename.ext)

  #------------------------------------

  ### textInput()
  textInput("export_filename", tags$h5("Filename"), value = filename.value)
})


###############################################################################
output$export_weight_inc_uiOut_text <- renderUI({
  req(length(vals$models.ll) > 0)
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected
  req(sum(!sapply(list(x, y, z), is.null)) == 1)

  if (isTruthy(z)) {
    tags$h5("Ensemble predictions do not have any weight data to export")

  } else {
    data.w <- st_set_geometry(export_model_selected(), NULL)

    if ("Weight" %in% names(data.w)) {
      tags$h5("The selected predictions have weight data",
              "that will be exported")
    } else {
      tags$h5("The selected predictions do not have any weight data to export")
      }
  }
})


###############################################################################
### Download button to export predictions
output$export_out_uiOut_download <- renderUI({
  req(input$export_filename)

  isolate({
    ext.text <- switch(
      as.numeric(input$export_format),
      ".csv", ".shp", ifelse(input$export_format_kml == 1, ".kml", ".kmz")
    )
  })

  validate(
    need(substr_right(input$export_filename, 4) == ext.text,
         paste("Error: The file extension of the filename must match the",
               "file extension required by the specified file format"))
  )

  export_crs() #to get validate()

  downloadButton("export_out", "Export predictions")
})

###############################################################################
