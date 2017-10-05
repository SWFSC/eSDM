### server.R for the Ensemble Tool for Species Distribution Modeling (eSDM)
# Designed by Sam Woodman


###############################################################################
### Naming convention
# File names: 'server/ui' + 'tab number' + camelCase + description
# File names (cont): '_' used as separator
# Separator for variable and reactive function names: '_'
# Separator for variables that aren't being passed from server to ui: '.'
#    This includes reactiveValues
# Separator for non-reactive functions: '.'
# renderUI output names for widgets: 'output$inputId_uiOut_(widget type)'


###############################################################################
### Install packages (if necessary), including packages loaded in ui.R
# Credits: https://gist.github.com/benmarwick/5054846 via
#          https://stackoverflow.com/questions/4090169
list.of.packages <- c("dplyr", "sp", "rgdal", "rgeos", "raster", "cleangeo", 
                      "lattice", "gridExtra", "RCurl", "ROCR", "DT", 
                      "colorRamps",  "RColorBrewer", "viridis", "dichromat", 
                      "sendmailR", "shiny", "shinyjs", "shinydashboard", 
                      "shinycssloaders")

list.of.packages.tf <- list.of.packages %in% installed.packages()[, "Package"]
new.packages <- list.of.packages[!list.of.packages.tf]
if (length(new.packages)) install.packages(new.packages) 


###############################################################################
### Load packages
library(dplyr) # Loaded first so dplyr functions those that are masked
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(cleangeo)
library(lattice)
library(gridExtra)
library(RCurl)
library(ROCR)
library(DT)
library(colorRamps)
library(RColorBrewer)
library(viridis)
library(dichromat)
library(sendmailR)


###############################################################################
# Pre-server work

### Max file upload size is now 150MB
options(shiny.maxRequestSize = 150 * 1024^2) 

### Use to perform sequential rather than concurrent validate checks
`%then%` <- shiny:::`%OR%`


###############################################################################
### Server function
server <- function(input, output, session) {
  ### Quit App
  observeEvent(input$close_app, {
    stopApp(returnValue = "Ensemble app was closed")
  })
  
  
  ### Load all other server code: tab-specific scripts and general server code
  source(file.path("server_other", "server_reactiveValues.R"), local = TRUE, chdir = TRUE)
  
  source("temp.R", local = TRUE, chdir = TRUE)
  
  # Load model predictions
  source(file.path("server_1_loadModels", "server_1_loadModels.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_csv.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_raster.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_shpgdb.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_renderUI.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_funcs.R"), local = TRUE, chdir = TRUE)
  
  
  # Overlay model predictions
  source(file.path("server_2_overlay", "server_2_overlay.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_csv.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_shpgdb.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_provided.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels_base.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_renderUI.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_funcs.R"), local = TRUE, chdir = TRUE)
  
  
  # Create ensemble predictions
  source(file.path("server_3_createEns", "server_3_createEns.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create_weighted.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create_weighted_poly.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_renderUI.R"), local = TRUE, chdir = TRUE)
  
  
  # Calculate evaluation metrics
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_loadData.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_renderUI.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_funcs.R"), local = TRUE, chdir = TRUE)
  
  
  # Make high quality maps (pretty plots)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_prep.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_plot.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_download.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_renderUI.R"), local = TRUE, chdir = TRUE)
  
  
  # Export model predictions
  source(file.path("server_6_export", "server_6_export.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_6_export", "server_6_export_renderUI.R"), local = TRUE, chdir = TRUE)
  
  
  # Manual
  # The function tags$iframe(...) is in ui.R so that the manual renders immediately
  
  
  # Submit feedback
  source(file.path("server_8_feedbackForm", "server_8_feedbackForm.R"), local = TRUE, chdir = TRUE)
  
  
  # General server code
  source(file.path("server_other", "server_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_variables.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots_download.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots_funcs.R"), local = TRUE, chdir = TRUE)
  # server_reactiveValues.R is sourced at the top of the server code in order to initialize reactiveValues
  source(file.path("server_other", "server_render.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_render_tables.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_hide+show.R"), local = TRUE, chdir = TRUE)
  
  
  ### Hide plot outputs when app is first started
  shinyjs::hide("model_pix_preview_plot", time = 0)
  shinyjs::hide("overlay_preview_base", time = 0)
  shinyjs::hide("overlay_preview_overlaid", time = 0)
  shinyjs::hide("create_ens_weights_poly_preview_plot", time = 0)
  shinyjs::hide("ens_pix_preview_plot", time = 0)
  shinyjs::hide("pretty_plot", time = 0)
}

###############################################################################
