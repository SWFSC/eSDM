### renderUI()'s for eval metrics tab
# Error messages for pres/abs code widgets is done via flags in ..._loadData.R


###############################################################################
# ui's for single file csv

### Select long, lat, and data column names
output$eval_csv_names_1_uiOut_select <- renderUI({
  csv.info <- eval_data_1_csv_load()
  req(csv.info)
  
  choice.input.names <- names(csv.info[[2]])
  choice.input <- seq_along(choice.input.names)
  names(choice.input) <- choice.input.names
  
  selectizeInput("eval_csv_names_1", 
                 tags$h5("Select, in this order, the longitude, latitude, and", 
                         "validation data column for the uploaded .csv file"), 
                 choices = choice.input, selected = NULL, multiple = TRUE)
})

### Select presence codes
output$eval_csv_codes_1_p_uiOut_select <- renderUI({
  choice.input.names <- eval_data_1_csv_pacodes()
  req(!any(c("error1", "error2") %in% eval_data_1_csv_pacodes()))
  
  selectizeInput("eval_csv_codes_1_p", tags$h5("Select presence code(s)"), 
                 choices = choice.input.names, selected = NULL, 
                 multiple = TRUE)
})

### Select absence codes
output$eval_csv_codes_1_a_uiOut_select <- renderUI({
  choice.input.names <- eval_data_1_csv_pacodes()
  req(!any(c("error1", "error2") %in% eval_data_1_csv_pacodes()))
  
  selectizeInput("eval_csv_codes_1_a", tags$h5("Select absence code(s)"), 
                 choices = choice.input.names, selected = NULL, 
                 multiple = TRUE)
})

### Click to load csv validation data
output$eval_csv_execute_1_uiOut_button <- renderUI({
  if (input$eval_data_type_1 == 1) {
    req(eval_data_1_csv_load(), length(input$eval_csv_names_1) == 3)
  } else {
    req(!any(c("error1", "error2") %in% eval_data_1_csv_pacodes()))
  }
  
  actionButton("eval_csv_execute_1", "Load specified validation data into app")
})


###############################################################################
# ui's for single file GIS

### Select data column name
output$eval_gis_names_1_uiOut_select <- renderUI({
  pa.spdf.list <- vals$eval.data.gis.file.1
  req(length(pa.spdf.list) > 0)
  pa.spdf <- pa.spdf.list[[1]]
  req(pa.spdf, pa.spdf.list[[2]] == input$eval_load_type_1)
  
  choice.input.names <- names(pa.spdf)
  choice.input <- seq_along(choice.input.names)
  names(choice.input) <- choice.input.names
  
  selectInput("eval_gis_names_1", 
              tags$h5("Select the validation data column for the", 
                      "uploaded GIS file"), 
              choices = choice.input, selected = NULL)
})

### Select presence codes
output$eval_gis_codes_1_p_uiOut_select <- renderUI({
  choice.input.names <- eval_data_1_gis_pacodes()
  req(!("error2" %in% choice.input.names))
  
  selectizeInput("eval_gis_codes_1_p", tags$h5("Select presence code(s)"), 
                 choices = choice.input.names, selected = NULL, 
                 multiple = TRUE)
})

### Select absence codes
output$eval_gis_codes_1_a_uiOut_select <- renderUI({
  choice.input.names <- eval_data_1_gis_pacodes()
  req(!("error2" %in% choice.input.names))
  
  selectizeInput("eval_gis_codes_1_a", tags$h5("Select absence code(s)"), 
                 choices = choice.input.names, selected = NULL, 
                 multiple = TRUE)
})

### Click to load GIS validation data
output$eval_gis_execute_1_uiOut_button <- renderUI({
  if (input$eval_data_type_1 == 1) {
    pa.spdf.list <- vals$eval.data.gis.file.1
    req(length(pa.spdf.list) > 0)
    pa.spdf <- pa.spdf.list[[1]]
    req(pa.spdf, pa.spdf.list[[2]] == input$eval_load_type_1)
  } else {
    req(!("error2" %in% eval_data_1_gis_pacodes()))
  }
  
  actionButton("eval_gis_execute_1", "Load specified validation data into app")
})


###############################################################################
### Choice of metric(s) to calculate
output$eval_metrics_which_uiOut_check <- renderUI({
  req(vals$eval.data.specs)
  
  choices.list <- list("AUC", "TSS", "RMSE")
  if (vals$eval.data.specs == 2) choices.list <- choices.list[1:2]  
  
  checkboxGroupInput("eval_metrics_which", tags$h5("Metric(s) to calculate"), 
                     choices = choices.list)
})
