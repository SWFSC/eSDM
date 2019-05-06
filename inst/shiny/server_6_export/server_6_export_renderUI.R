### renderUI code for Export Predictions tab


###############################################################################
### Predictions with coord system in which to export selected predictions
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
    # Original predictions
    table.info <- table_orig()[x, ]
    filename.value <- paste(table.info[, 1:2], collapse = "__")
    filename.value <- paste0(filename.value, "__orig")

  } else if (isTruthy(y)) {
    # Overlaid predictions
    table.info <- table_overlaid()[y, ]
    filename.value <- paste(table.info[, 1:2], collapse = "__")
    filename.value <- paste0(filename.value, "__overlaid")

  } else {  # isTruthy(z)
    # Ensemble predictions
    table.info <- table_ensembles()[z, ]

    table.info$`Predictions used` <- switch(
      table.info$`Predictions used`,
      "All overlaid" = "All", substring( table.info$`Predictions used`, 10)
    )

    rescale.txt <- table.info$`Rescaling method`
    rescale.txt <- ifelse(
      grepl("Abund", rescale.txt),
      paste0("Abund", strsplit(rescale.txt, ": ")[[1]][2]),
      switch(
        rescale.txt, "None" = "None", "Sum to 1" = "Sumto1"
      )
    )
    table.info$`Rescaling method` <- rescale.txt

    table.info$`Uncertainty method` <- ifelse(
      table.info$`Uncertainty method` == "Among-model", "AMV", "WMV"
    )

    filename.value <- paste(table.info, collapse = "_")
    filename.value <- gsub(", ", "+", filename.value) #Change comma to '+'
    filename.value <- gsub(" ", "", filename.value)   #Gets rid of spaces
  }

  #------------------------------------
  ### Projection info
  if (input$export_proj_360) filename.value <- paste0(filename.value, "_360")

  #------------------------------------
  ### Prefix
  filename.value <- paste0("eSDM_", gsub("\\.", "_", filename.value))

  #------------------------------------

  input.lab <- "Filename (without file extension)"
  textInput("export_filename", tags$h5(input.lab), value = filename.value)
})


###############################################################################
### Message about whether predictions have weight data
output$export_weight_inc_uiOut_text <- renderUI({
  req(length(vals$models.ll) > 0)
  x <- input$export_table_orig_out_rows_selected
  y <- input$export_table_over_out_rows_selected
  z <- input$export_table_ens_out_rows_selected
  req(sum(!sapply(list(x, y, z), is.null)) == 1)

  if (isTruthy(z)) {
    tags$h5("Ensemble predictions do not have any weight data to export,",
            "and thus the downloaded file will not contain any weight data")
  } else {
    NULL
  }
})


###############################################################################
### Download button to export predictions
output$export_out_uiOut_download <- renderUI({
  validate(
    need(!identical(input$export_filename, ""),
         "Error: Please enter a filename")
  )
  req(input$export_filename)

  export_crs() #to get validate()

  downloadButton("export_out", "Export predictions")
})

###############################################################################
