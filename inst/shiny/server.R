### server.R for the Ensemble Tool for Species Distribution Modeling (eSDM)
# Designed by Sam Woodman


###############################################################################
### Naming convention
# File names: 'server/ui' + 'tab number' + camelCase + description
# File names (cont): '_' used as separator
# Separator for variable and function names: '_'
# Separator for variables that aren't being passed from server to ui: '.'
#    This includes reactiveValues
# renderUI output names for widgets: 'output$inputId_uiOut_(widget type)'


###############################################################################
library(colorRamps)
library(colourpicker)
library(dichromat)
library(dplyr)
library(DT)
library(eSDM)
library(leaflet)
library(lwgeom)
library(mapview)
library(purrr)
library(raster)
library(RColorBrewer)
library(ROCR)
library(sf)
library(viridis)


###############################################################################
# Pre-server work

### Max file upload size is now 150MB
options(shiny.maxRequestSize = 150 * 1024^2)

### Use to perform sequential rather than concurrent validate checks
`%then%` <- shiny:::`%OR%`

### Server-wide CRS code(s)
crs.ll <- st_crs(4326) # WGS 84
# crs.cea <- st_crs(
#   paste("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m",
#         "+no_defs +ellps=WGS84 +towgs84=0,0,0")
# )
# # crs.cea is proj4string from st_crs(boundary.proj.poly), which had
# #   Projected Coordinate System 'World_Cylindrical_Equal_Area'in GIS

### For plotting
pal.esdm <- c(
  "#313695", "#4575b4", "#74add1", "#abd9e9", "#d1e5f0", "#fee090",
  "#fdae61", "#f46d43", "#d73027", "#a50026"
)
leg.perc.esdm <- c(
  "Lowest 60%", "35 - 40%", "30 - 35%", "25 - 30%", "20 - 25%", "15 - 20%",
  "10 - 15%", "5 - 10%", "2 - 5%", "Highest 2%"
)


###############################################################################
### Server function
server <- function(input, output, session) {
  ###############################################
  ### Quit App
  observeEvent(input$close_app, {
    stopApp(returnValue = "Ensemble app was closed")
  })

  ###############################################
  ### Load all other server code: tab-specific scripts and general server code
  source(file.path("server_other", "server_reactiveValues.R"), local = TRUE, chdir = TRUE)

  # Roadmap: download sample data
  output$download_sample_data <- downloadHandler(
    filename = function() {
      "eSDM_sample_data.zip"
    },
    content = function(file) {
      withProgress(message = "Downloading sample data", value = 0.6, {
        sample.try <- try(download.file("https://github.com/smwoodman/eSDM/raw/master/inst/extdata/data_provided.zip",
                                        destfile = file, quiet = TRUE),
                          silent = TRUE)
        validate(
          need(isTruthy(sample.try),
               paste("Sample data could not be downloaded; please check your",
                     "internet connection. If this problem persists,",
                     "email Sam Woodman (sam.woodman@noaa.gov) or",
                     "Karin Forney (karin.forney@noaa.gov)."))
        )
        incProgress(0.4)
      })
    }
  )

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
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels_samegrid.R"), local = TRUE, chdir = TRUE)
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


  # # Submit feedback
  # source(file.path("server_8_feedbackForm", "server_8_feedbackForm.R"), local = TRUE, chdir = TRUE)


  # General server code
  source(file.path("server_other", "server_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots_download.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots_funcs.R"), local = TRUE, chdir = TRUE)
  # server_reactiveValues.R is sourced at the top of the server code in order to initialize reactiveValues
  source(file.path("server_other", "server_render.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_render_tables.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_hide+show.R"), local = TRUE, chdir = TRUE)
}

###############################################################################
