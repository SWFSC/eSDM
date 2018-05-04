### Load and store validation data
### Note: in functions, numeric (count) validation data is often referred to...
### ...as 'presence' and 'absence' data


###############################################################################
# Flags for conditionalPanel()'s

###########################################################
### CSV error message and inputs
output$eval_csv_1_flag <- reactive({
  !is.null(eval_data_1_csv_load())
})
outputOptions(output, "eval_csv_1_flag", suspendWhenHidden = FALSE)


###########################################################
### GIS error messages
output$eval_gis_1_shp_flag <- reactive({
  !is.null(eval_data_1_shp_load())
})
outputOptions(output, "eval_gis_1_shp_flag", suspendWhenHidden = FALSE)

output$eval_gis_1_gdb_flag <- reactive({
  !is.null(eval_data_1_gdb_load())
})
outputOptions(output, "eval_gis_1_gdb_flag", suspendWhenHidden = FALSE)


###########################################################
# Messages for invalid number/type of column selection
# Ouputs only control error messages; rendered widgets use req() statements

### CSV
output$eval_csv_1_error_flag <- reactive({
  if (input$eval_data_type_1 == 1) {
    if (length(input$eval_csv_names_1) != 3) {
      1
    } else {
      FALSE  
    }
  } else {
    code.flag <- eval_data_1_csv_pacodes()
    if ("error1" %in% code.flag) {
      1
    } else if ("error2" %in% code.flag) {
      2
    } else {
      FALSE
    }
  }
})
outputOptions(output, "eval_csv_1_error_flag", suspendWhenHidden = FALSE)

### GIS
output$eval_gis_1_error_flag <- reactive({
  if (input$eval_data_type_1 == 1) {
    FALSE
  } else {
    code.flag <- eval_data_1_gis_pacodes()
    if ("error2" %in% code.flag) { #"error2" to keep consistent with csv
      2
    } else {
      FALSE
    }
  }
})
outputOptions(output, "eval_gis_1_error_flag", suspendWhenHidden = FALSE)


###########################################################
### Display GIS 'general' input if data has been loaded from shp and...
# ...shp is selected, or if same for gdb
output$eval_gis_1_flag <- reactive({
  req(length(vals$eval.data.gis.file.1) != 0)

  x <- try((!is.null(eval_data_1_shp_load()) & input$eval_load_type_1 == 2 & 
              vals$eval.data.gis.file.1[[2]] == 2), 
           silent = TRUE)
  y <- try((!is.null(eval_data_1_gdb_load()) & input$eval_load_type_1 == 3 & 
              vals$eval.data.gis.file.1[[2]] == 3), 
           silent = TRUE)
  
  if(class(x) == "try-error") x <- FALSE
  if(class(y) == "try-error") y <- FALSE
  
  x | y
})
outputOptions(output, "eval_gis_1_flag", suspendWhenHidden = FALSE)


###############################################################################
# Loading validation data: One file versions

###########################################################
# Loading from one csv

### Read in .csv file
eval_data_1_csv_load <- reactive({
  file.in <- input$eval_csv_1
  req(file.in)
  
  if (!(file.in$type %in% c("text/csv", "application/vnd.ms-excel"))) return()
  
  csv.data <- read.csv(file.in$datapath, stringsAsFactors = FALSE)
  return (list(file.in$name, csv.data))
})

### Get options for presence/absence codes
eval_data_1_csv_pacodes <- reactive({
  csv.info <- eval_data_1_csv_load()
  req(csv.info, input$eval_data_type_1 == 2)
  
  if (length(input$eval_csv_names_1) != 3) return("error1")
  
  col.pa <- as.numeric(input$eval_csv_names_1[3])
  choice.input.names <- unique(csv.info[[2]][, col.pa])
  choice.input.names <- choice.input.names[order(choice.input.names)]
  
  if (length(choice.input.names) > 50) return("error2") 
  
  choice.input.names
})

### Process and then save validation data to reactiveVal
eval_data_1_csv <- eventReactive(input$eval_csv_execute_1, {
  req(eval_data_1_csv_load(), length(input$eval_csv_names_1) == 3)
  
  data.type <- as.numeric(input$eval_data_type_1)
  csv.all <- eval_data_1_csv_load()[[2]]
  columns.all <- as.numeric(input$eval_csv_names_1)
  
  csv.selected <- csv.all[ ,columns.all]
  
  if (data.type == 1) { ## Count data
    validate(
      need(is.numeric(csv.selected[,3]) | is.integer(csv.selected[,3]), 
           paste("Error: Selected validation data column is not numeric.", 
                 "Consider loading data as 'Presence or absence' data"))
    )
    
    p.data <- csv.selected[csv.selected[,3] >  0, ]
    a.data <- csv.selected[csv.selected[,3] == 0, ]
    
  } else { ## Presence/absence data
    p.codes <- input$eval_csv_codes_1_p
    a.codes <- input$eval_csv_codes_1_a
    num.codes <- length(p.codes) + length(a.codes)
    
    validate(
      need(!(is.null(p.codes) | is.null(a.codes)), 
           paste("Error: Please select one or more", 
                 "presence codes and absence codes")), 
      need(all(!(p.codes %in% a.codes)), 
           paste("Error: Please ensure that no presence and", 
                 "absence codes are the same")), 
      need(length(unique(csv.selected[,3])) <= num.codes, 
           paste("Error: Please ensure that all codes are classified", 
                 "as either presence or absence codes"))
    )
    
    p.data <- csv.selected[csv.selected[,3] %in% p.codes, ]
    a.data <- csv.selected[csv.selected[,3] %in% a.codes, ]
  }
  
  # Convert points to spdf, function in server_4_evalMetrics_funcs.R
  p.spdf <- pa.data.csv.process(p.data, 1, data.type)
  a.spdf <- pa.data.csv.process(a.data, 0, data.type)
  
  vals$eval.data.list <- list(p.spdf, a.spdf)
  vals$eval.data.specs <- data.type
  
  return ("Loaded and stored validation data")
})


###########################################################
# Loading one file versions from GIS

#######################################
### Load validation shp and save it to reactiveValue
eval_data_1_shp_load <- reactive({
  req(input$eval_gis_shp_1)
  
  shp.files <- input$eval_gis_shp_1
  
  withProgress(message = "Loading GIS file", value = 0.3, {
    gis.file.shp <- read.shp.in(shp.files)
    gis.file.success <- isTruthy(gis.file.shp)
    incProgress(0.4)
    
    if (gis.file.success) { #keep crs.ll spdf
      gis.file.shp.ll <- gis.model.check(gis.file.shp)[[1]]
    }
    incProgress(0.3)
  })
  
  if (!gis.file.success) {
    NULL
  } else {
    gis.file.shp.ll
  }
})

observe({
  req(eval_data_1_shp_load())
  vals$eval.data.gis.file.1 <- list(eval_data_1_shp_load(), 2)
})


#######################################
### Load validation gdb and save it to reactiveVar
eval_data_1_gdb_load <- eventReactive(input$eval_gis_gdb_load_1, {
  gdb.path <- input$eval_gis_gdb_path_1
  gdb.name <- input$eval_gis_gdb_name_1
  
  withProgress(message = "Loading GIS file", value = 0.3, {
    gis.file.gdb <- try(readOGR(gdb.path, gdb.name, verbose = FALSE), 
                        silent = TRUE)
    gis.file.success <- isTruthy(gis.file.gdb)
    incProgress(0.4)
    
    if (gis.file.success) gis.file.gdb.ll <- gis.model.check(gis.file.gdb)[[1]] #keep crs.ll version
    incProgress(0.3)
  })
  
  if (!gis.file.success) {
    NULL
  } else {
    gis.file.gdb.ll
  }
})

observe({
  req(eval_data_1_gdb_load())
  vals$eval.data.gis.file.1 <- list(eval_data_1_gdb_load(), 3)
  shinyjs::reset("eval_gis_shp_1")
})


#######################################
### Get options for presence/absence codes
eval_data_1_gis_pacodes <- reactive({
  pa.spdf.list <- vals$eval.data.gis.file.1
  req(length(pa.spdf.list) > 0)
  pa.spdf <- pa.spdf.list[[1]]
  req(pa.spdf, pa.spdf.list[[2]] == input$eval_load_type_1)
  
  col.pa <- as.numeric(input$eval_gis_names_1)
  choice.input.names <- unique(pa.spdf@data[, col.pa])
  choice.input.names <- choice.input.names[order(choice.input.names)]
  
  if (length(choice.input.names) > 50) return("error2")
  # "error2' to keep consistent with csv error code
  
  choice.input.names
})

#######################################
### GIS process and then save validation data to reactiveVar
eval_data_1_gis <- eventReactive(input$eval_gis_execute_1, {
  column.data <- as.numeric(input$eval_gis_names_1)
  
  pa.spdf <- vals$eval.data.gis.file.1[[1]]
  pa.spdf@data <- data.frame(pa.data = pa.spdf@data[,column.data])
  data.type <- as.numeric(input$eval_data_type_1)
  
  # Sort data by lat/long with lat as primary for bottom-up sort
  coords.spdf <- coordinates(pa.spdf)
  pa.spdf <- pa.spdf[order(coords.spdf[,1]), ] #Lon first so lat is primary
  pa.spdf <- pa.spdf[order(coords.spdf[,2]), ]
  
  # Split data into p/a spdfs and add sight column
  if (data.type == 1) {
    ## Count data
    validate(
      need(is.numeric(pa.spdf$pa.data) | is.integer(pa.spdf$pa.num), 
           paste("Error: Selected data column is not numeric.", 
                 "Consider loading data as 'presence/absence' data"))
    )
    
    names(pa.spdf) <- "pa.num"
    
    p.spdf <- pa.spdf[pa.spdf$pa.num >  0, ]
    a.spdf <- pa.spdf[pa.spdf$pa.num == 0, ]
    
  } else {
    ## Presence/absence data
    p.codes <- input$eval_gis_codes_1_p
    a.codes <- input$eval_gis_codes_1_a
    num.codes <- length(p.codes) + length(a.codes)
    
    validate(
      need(!(is.null(p.codes) | is.null(a.codes)), 
           paste("Error: Please select one or more", 
                 "presence code and absence code")) %then%
      need(all(!(p.codes %in% a.codes)), 
           paste("Error: Please ensure that no presence and", 
                 "absence codes are the same")), 
      need(length(unique(pa.spdf$pa.data)) <= num.codes, 
           paste("Error: Please ensure that all codes are classified", 
                 "as either presence or absence codes"))
    )

    p.spdf <- pa.spdf[pa.spdf$pa.data %in% p.codes, ]
    a.spdf <- pa.spdf[pa.spdf$pa.data %in% a.codes, ]
    
    p.spdf$pa.data <- NA
    a.spdf$pa.data <- NA
    
    names(p.spdf) <- "pa.num"
    names(a.spdf) <- "pa.num"
  }
  
  p.spdf$pa.sight <- rep(1, length(p.spdf))
  a.spdf$pa.sight <- rep(0, length(a.spdf))
  
  vals$eval.data.list <- list(p.spdf, a.spdf)
  vals$eval.data.specs <- data.type
  
  return("Loaded and stored validation data")
})


###############################################################################
