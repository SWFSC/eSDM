### server.R for eSDM GUI by Sam Woodman
# Note 'importing predictions' was 'loading models' when code was first written


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
library(methods)
library(purrr)
library(raster)
library(RColorBrewer)
library(rlang)
library(ROCR)
library(sf)
library(shinyjs)
library(stats)
library(tmap)
library(units)
library(viridis)


###############################################################################
# Pre-server work

### ax file upload size is now 150MB
options(shiny.maxRequestSize = 150 * 1024^2)

### Use to perform sequential rather than concurrent validate checks
`%then%` <- shiny:::`%OR%`

### Server-wide CRS code
crs.ll <- st_crs(4326) # WGS 84

### Plotting variables
pal.esdm <- c(
  "#313695", "#4575b4", "#74add1", "#abd9e9", "#d1e5f0",
  "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"
)
# Use rev(RColorBrewer::brewer.pal(10, "Spectral")) for values plots
leg.perc.esdm <- c(
  "Lowest 60%", "35 - 40%", "30 - 35%", "25 - 30%", "20 - 25%",
  "15 - 20%", "10 - 15%", "5 - 10%", "2 - 5%", "Highest 2%"
)


###############################################################################
### Server function
server <- function(input, output, session) {
  #----------------------------------------------------------------------------
  ### Quit GUI
  session$onSessionEnded(function() {
    stopApp(returnValue = "eSDM GUI was closed")
  })

  observeEvent(input$close_gui, {
    js$closeWindow()
    stopApp(returnValue = "eSDM GUI was closed")
  })

  observeEvent(input$close_gui_error, {
    js$closeWindow()
    stopApp(returnValue = "eSDM GUI was closed due to a data storage and processing error")
  })

  #----------------------------------------------------------------------------
  # Source general and tab-specific server code

  ### General server code
  source(file.path("server_other", "server_checks.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_funcs_preview360.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots_download.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_plots_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_reactiveValues.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_render.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_tables.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_other", "server_workspace.R"), local = TRUE, chdir = TRUE)


  ### Roadmap
  # Download sample data
  output$download_sample_data <- downloadHandler(
    filename = "eSDM_sample_data.zip",

    content = function(file) {
      withProgress(message = "Downloading sample data", value = 0.6, {
        sample.try <- try(
          download.file(
            "https://github.com/smwoodman/eSDM-data/raw/master/data_provided.zip",
            destfile = file, quiet = TRUE
          ),
          silent = TRUE
        )

        validate(
          need(sample.try,
               paste("The sample data could not be downloaded; please check",
                     "your internet connection. If this problem persists, please",
                     "report this issue at https://github.com/smwoodman/eSDM/issues"))
        )
        incProgress(0.4)
      })
    }
  )

  # Download manual
  output$download_manual <- downloadHandler(
    filename = "eSDM_manual.pdf",

    content = function(file) {
      withProgress(message = "Downloading manual", value = 0.6, {
        sample.try <- try(
          download.file(
            "https://github.com/smwoodman/eSDM/raw/master/inst/shiny/www/eSDM_manual.pdf",
            destfile = file, quiet = TRUE, mode = "wb"
          ),
          silent = TRUE
        )

        validate(
          need(sample.try,
               paste("The manual could not be downloaded; please check",
                     "your internet connection. If this problem persists, please",
                     "report this issue at https://github.com/smwoodman/eSDM/issues"))
        )
        incProgress(0.4)
      })
    }
  )


  ### Import predictions (previously called 'load model predictions')
  source(file.path("server_1_loadModels", "server_1_loadModels.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_csv.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_raster.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_renderUI.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_shpgdb.R"), local = TRUE, chdir = TRUE)


  ### Overlay predictions
  source(file.path("server_2_overlay", "server_2_overlay.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_csv.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_shpgdb.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_provided.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels_base.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_renderUI.R"), local = TRUE, chdir = TRUE)


  ### Create ensemble predictions
  source(file.path("server_3_createEns", "server_3_createEns.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create_regionalweighting.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create_weighted.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_renderUI.R"), local = TRUE, chdir = TRUE)


  ### Calculate evaluation metrics
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_loadData.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_renderUI.R"), local = TRUE, chdir = TRUE)


  ### Make high quality maps (pretty plots)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_addobj.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_addobj_renderUI.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_addobj_update.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_download.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_plot.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_prep.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_renderUI.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_toplot.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_update.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_5_prettyPlot", "server_5_prettyPlot_update_renderUI.R"), local = TRUE, chdir = TRUE)


  ### Export predictions
  source(file.path("server_6_export", "server_6_export.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_6_export", "server_6_export_renderUI.R"), local = TRUE, chdir = TRUE)


  ### Manual
  # The function tags$iframe(...) is in ui.R so that the manual renders immediately
}

###############################################################################
