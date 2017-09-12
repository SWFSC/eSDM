### renderUI()'s for eval metrics tab


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
                 h5("Select, in this order, the longitude, latitude,",
                          "and data column for", csv.info[[1]]),
                 choices = choice.input, selected = NULL, multiple = TRUE)
})

### Select presence codes
output$eval_csv_codes_1_p_uiOut_select <- renderUI({
  csv.info <- eval_data_1_csv_load()
  req(csv.info)
  
  validate(
    need(length(input$eval_csv_names_1) == 3, 
         paste("Please select 3 columns in order to", 
               "select presence and absence codes"))
  )
  
  col.pa <- as.numeric(input$eval_csv_names_1[3])
  choice.input.names <- unique(csv.info[[2]][,col.pa])
  incProgress(0.6)
  choice.input.names <- choice.input.names[order(choice.input.names)]
  
  selectizeInput("eval_csv_codes_1_p", h5("Select presence code(s)"), 
                 choices = choice.input.names, selected = NULL, multiple = TRUE)
})

### Select absence codes
output$eval_csv_codes_1_a_uiOut_select <- renderUI({
  csv.info <- eval_data_1_csv_load()
  req(csv.info, length(input$eval_csv_names_1) == 3)

  col.pa <- as.numeric(input$eval_csv_names_1[3])
  choice.input.names <- unique(csv.info[[2]][,col.pa])
  choice.input.names <- choice.input.names[order(choice.input.names)]
  
  selectizeInput("eval_csv_codes_1_a", h5("Select absence code(s)"), 
                 choices = choice.input.names, selected = NULL, multiple = TRUE)
})

### Click to load csv validation data
output$eval_csv_execute_1_uiOut_button <- renderUI({
  req(eval_data_1_csv_load(), length(input$eval_csv_names_1) == 3)
  
  actionButton("eval_csv_execute_1", "Save selected csv p/a info")
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
  
  selectInput("eval_gis_names_1", h5("Select the p/a column for the loaded gis file"),
              choices = choice.input, selected = 6)
})

### Select presence codes
output$eval_gis_codes_1_p_uiOut_select <- renderUI({
  pa.spdf.list <- vals$eval.data.gis.file.1
  req(length(pa.spdf.list) > 0)
  pa.spdf <- pa.spdf.list[[1]]
  req(pa.spdf, pa.spdf.list[[2]] == input$eval_load_type_1)
  
  col.pa <- as.numeric(input$eval_gis_names_1)
  choice.input.names <- unique(pa.spdf@data[,col.pa])
  choice.input.names <- choice.input.names[order(choice.input.names)]
  
  selectizeInput("eval_gis_codes_1_p", h5("Select presence code(s)"),
                 choices = choice.input.names, selected = NULL, multiple = TRUE)
})

### Select absence codes
output$eval_gis_codes_1_a_uiOut_select <- renderUI({
  pa.spdf.list <- vals$eval.data.gis.file.1
  req(length(pa.spdf.list) > 0)
  pa.spdf <- pa.spdf.list[[1]]
  req(pa.spdf, pa.spdf.list[[2]] == input$eval_load_type_1)
  
  col.pa <- as.numeric(input$eval_gis_names_1)
  choice.input.names <- unique(pa.spdf@data[,col.pa])
  choice.input.names <- choice.input.names[order(choice.input.names)]
  
  selectizeInput("eval_gis_codes_1_a", h5("Select absence code(s)"),
                 choices = choice.input.names, selected = NULL, multiple = TRUE)
})

### Click to load gis validation data
output$eval_gis_execute_1_uiOut_button <- renderUI({
  pa.spdf.list <- vals$eval.data.gis.file.1
  req(length(pa.spdf.list) > 0)
  pa.spdf <- pa.spdf.list[[1]]
  req(pa.spdf, pa.spdf.list[[2]] == input$eval_load_type_1)
  
  actionButton("eval_gis_execute_1", "Save selected gis p/a info")
})


###############################################################################
### Choice of metric(s) to calculate
output$eval_metrics_which_uiOut_check <- renderUI({
  req(vals$eval.data.specs)
  
  choices.list <- list("AUC", "TSS", "RMSE")
  if (vals$eval.data.specs == 2) choices.list <- choices.list[1:2]  
  
  checkboxGroupInput("eval_metrics_which", h5("Metric(s) to calculate"), 
                     choices = choices.list)
})
