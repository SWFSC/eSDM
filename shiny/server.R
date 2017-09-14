### server.R for Ensemble Shiny App
# Designed by Sam Woodman


###############################################################################
### Naming convention
# File names are camelCase + '_' + ...
# Use '_' as separator for variable and reactive function names
# Use '.' as separator for variables that aren't being passed 
#     from server to ui, including reactive values
# USe '.' as separator for non-reactive functions
# renderUI output names for widgets are 'output$inputId_uiOut_(widget type)'


###############################################################################
library(dplyr)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(cleangeo)
library(lattice)
library(gridExtra)
library(colorRamps)
library(ROCR)
library(DT)

# Max file upload size is now 200MB for fine-scale land maps
options(shiny.maxRequestSize = 200 * 1024^2) 

`%then%` <- shiny:::`%OR%` # For non-simultaneous validate checks

###############################################################################
### Server function
server <- function(input, output, session) {
  ### Quit App
  observeEvent(input$close_app, {
    stopApp(returnValue = "Ensemble app was closed")
  })
  
  
  ### Load all other server code: tab-specific scripts and general server code
  source(file.path("server_other", "server_reactiveValues.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  
  source("temp.R", local = TRUE, echo = FALSE, chdir = TRUE)
  
  # Load/read models
  source(file.path("server_1_loadModels", "server_1_loadModels.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_csv.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_raster.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_shpgdb.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_renderUI.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_funcs.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  
  
  # Overlay process
  source(file.path("server_2_overlay", "server_2_overlay.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_csv.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_shpgdb.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_provided.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels_base.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels_func.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_renderUI.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  
  
  # Create simple ensemble
  source(file.path("server_3_createEns", "server_3_createEns.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create_weighted.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create_weighted_poly.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_renderUI.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  
  
  # Calculate evaluation metrics
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_loadData.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_renderUI.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_funcs.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  
  
  # Make high quality (pretty) plots
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_plot.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_download.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_renderUI.R"), local = TRUE, echo = FALSE, chdir = TRUE)

  
  # Export model predictions
  source(file.path("server_6_export", "server_6_export.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_6_export", "server_6_export_renderUI.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  
  
  # Manual
  output$manual_pdf <- renderUI({
    tags$iframe(style = "height:800px; width:100%", src = "Ensemble_app_manual.pdf")
  })
  
  
  # General server code
  source(file.path("server_other", "server_funcs+vars.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_other", "server_plots.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_other", "server_plots_download.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_other", "server_plots_funcs.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  # server_reactiveValues.R is sourced at the top of the server code in order to initialize reactiveValues
  source(file.path("server_other", "server_render.R"), local = TRUE, echo = FALSE, chdir = TRUE)
  source(file.path("server_other", "server_render_tables.R"), local = TRUE, echo = FALSE, chdir = TRUE)
}

###############################################################################
