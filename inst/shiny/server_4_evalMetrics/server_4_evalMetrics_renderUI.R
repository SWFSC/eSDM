#renderUI()'s for eval metrics tab
# Error messages for pres/abs code widgets is done via flags in ..._loadData.R


###############################################################################
# renderUI()'s for csv

### Select long, lat, and data column names
output$eval_csv_names_uiOut_select <- renderUI({
  csv.info <- eval_data_csv_load()
  req(csv.info)

  choice.input.names <- names(csv.info[[2]])
  choice.input <- seq_along(choice.input.names)
  names(choice.input) <- choice.input.names

  selectizeInput("eval_csv_names",
                 tags$h5("Select, in this order, the longitude, latitude, and",
                         "validation data column for the uploaded .csv file"),
                 choices = choice.input, selected = NULL, multiple = TRUE)
})

### Select presence codes
output$eval_csv_codes_p_uiOut_select <- renderUI({
  choice.input.names <- eval_data_csv_pacodes()
  req(!any(c("error1", "error2") %in% eval_data_csv_pacodes()))
  req(length(choice.input.names) > 1)

  selectizeInput("eval_csv_codes_p", tags$h5("Select presence code(s)"),
                 choices = choice.input.names, selected = NULL,
                 multiple = TRUE)
})

### Select absence codes
output$eval_csv_codes_a_uiOut_select <- renderUI({
  choice.input.names <- eval_data_csv_pacodes()
  req(!any(c("error1", "error2") %in% eval_data_csv_pacodes()))
  req(length(choice.input.names) > 1)

  selectizeInput("eval_csv_codes_a", tags$h5("Select absence code(s)"),
                 choices = choice.input.names, selected = NULL,
                 multiple = TRUE)
})

### Click to import csv validation data
output$eval_csv_execute_uiOut_button <- renderUI({
  if (input$eval_data_type == 1) {
    req(eval_data_csv_load(), length(input$eval_csv_names) == 3)

  } else {
    req(!any(c("error1", "error2") %in% eval_data_csv_pacodes()))
    validate(
      need(length(eval_data_csv_pacodes()) > 1,
           paste("The validation data column must contain at least two",
                 "unique values for it to be used as presence/absence data")),
      errorClass = "validation2"
    )
  }

  actionButton("eval_csv_execute", "Import validation data")
})


###############################################################################
# renderUI()'s for GIS

### Select data column name
output$eval_gis_names_uiOut_select <- renderUI({
  req(vals$eval.data.gis.info)
  req(vals$eval.data.gis.info[[1]],
      vals$eval.data.gis.info[[3]] == input$eval_load_type)

  choice.input.names <- names(
    st_set_geometry(vals$eval.data.gis.info[[2]], NULL)
  )
  choice.input <- seq_along(choice.input.names)
  names(choice.input) <- choice.input.names

  selectInput("eval_gis_names",
              tags$h5("Select the validation data column for the",
                      "uploaded object"),
              choices = choice.input, selected = NULL)
})

### Select presence codes
output$eval_gis_codes_p_uiOut_select <- renderUI({
  choice.input.names <- eval_data_gis_pacodes()
  req(!("error2" %in% choice.input.names))
  req(length(choice.input.names) > 1)

  selectizeInput("eval_gis_codes_p", tags$h5("Select presence code(s)"),
                 choices = choice.input.names, selected = NULL,
                 multiple = TRUE)
})

### Select absence codes
output$eval_gis_codes_a_uiOut_select <- renderUI({
  choice.input.names <- eval_data_gis_pacodes()
  req(!("error2" %in% choice.input.names))
  req(length(choice.input.names) > 1)

  selectizeInput("eval_gis_codes_a", tags$h5("Select absence code(s)"),
                 choices = choice.input.names, selected = NULL,
                 multiple = TRUE)
})

### Button to click to import GIS validation data
output$eval_gis_execute_uiOut_button <- renderUI({
  if (input$eval_data_type == 1) {
    req(vals$eval.data.gis.info)
    req(vals$eval.data.gis.info[[1]],
        vals$eval.data.gis.info[[3]] == input$eval_load_type)

  } else {
    req(!("error2" %in% eval_data_gis_pacodes()))
    validate(
      need(length(eval_data_gis_pacodes()) > 1,
           paste("The validation data column must contain at least two",
                 "unique values for it to be used as presence/absence data")),
      errorClass = "validation2"
    )
  }

  actionButton("eval_gis_execute", "Import validation data")
})


###############################################################################
### Choice of metric(s) to calculate
output$eval_metrics_which_uiOut_check <- renderUI({
  req(vals$eval.data.specs)

  choices.list <- list("AUC", "TSS", "RMSE")
  if (vals$eval.data.specs[[2]] == 2) choices.list <- choices.list[1:2]

  checkboxGroupInput("eval_metrics_which", tags$h5("Metric(s) to calculate"),
                     choices = choices.list)
})

###############################################################################
