### app.R for eSDM GUI by Sam Woodman


###############################################################################
# Check for and attach packages
stopifnot(require(eSDM))
list.packages <- list(
  "DT", "shiny", "shinybusy", "shinydashboard", "shinyjs",
  "colorRamps", "colourpicker", "dichromat", "dplyr", "leaflet", "leafem",
  "methods", "purrr", "RColorBrewer", "raster", "rlang", "ROCR",
  "sf", "stats", "tmap", "units", "viridis", "zip"
)

p.check <- vapply(list.packages, requireNamespace, as.logical(1), quietly = TRUE)
if (!all(p.check))
  stop("To use the eSDM GUI, the following packages must be installed: ",
       paste(list.packages, collapse = ", "), "\n",
       "To install the missing packages, run the following:\n",
       "install.packages(c(\"", paste(list.packages[!p.check],
                                    collapse = "\", \""), "\"))")

sapply(list.packages, require, character.only = TRUE)


###############################################################################
##### UI

#------------------------------------------------------------------------------
### Naming convention
# File names: 'server/ui' + 'tab number' + camelCase + description
# File names (cont): '_' used as separator
# Separator for variable and reactive function names: '_'
# Separator for variables that aren't being passed from server to ui: '.'
#    This includes reactiveValues
# Separator for non-reactive functions: '.'
# renderUI output names for widgets: 'output$inputId_uiOut_(widget type)'


### ui code (lists, instructions, etc) used in multiple tabs
source(file.path("ui_files", "ui_common.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_funcs.R"), local = TRUE, echo = FALSE, chdir = TRUE)

### ui code parsed by tabName
source(file.path("ui_files", "ui_0_roadmap.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_1_loadModels.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_2_overlay.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_3_createEns.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_4_evalMetrics.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_5_prettyPlot.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_6_export.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_7_manual.R"), local = TRUE, echo = FALSE, chdir = TRUE)


#------------------------------------------------------------------------------
### UI object for GUI
ui <- dashboardPage(
  skin = "blue",
  title = "eSDM GUI",
  dashboardHeader(
    title = "Ensemble tool for predictions from Species Distribution Models (eSDM)",
    titleWidth = "600px"
  ),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(HTML(paste0("eSDM GUI Roadmap and", "<br/>", "Load or Save Workspace")),
               tabName = "roadmap", icon = icon("road")), #icon("sitemap")
      menuItem("Import Predictions", tabName = "loadModels", icon = icon("cloud-upload")),
      menuItem("Overlay Predictions", tabName = "overlay", icon = icon("cogs")),
      menuItem("Create Ensemble Predictions", tabName = "createEns", icon = icon("cog")),
      menuItem("Evaluation Metrics", tabName = "evalMetrics", icon = icon("check")),
      menuItem("High Quality Maps", tabName = "prettyPlot", icon = icon("file-image-o")),
      menuItem("Export Predictions", tabName = "export", icon = icon("cloud-download")),
      menuItem("Manual", tabName = "manual", icon = icon("book"))
    ),
    tags$br(),
    actionButton("close_gui", label = "Close GUI")
  ),

  dashboardBody(
    ### Use shinyjs package and window-closing functionlity
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),

    ### Use shinybusy to indicate when plot work is being done
    shinybusy::add_busy_spinner(
      spin = "double-bounce", position = "top-right", margin = c(20, 20),
      height = "100px", width = "100px"
    ),


    ### Control validate text output
    tags$head(
      tags$style(HTML("
        .shiny-output-error-validation {
        color: red; font-weight: bold;
        }
      ")),
      tags$style(HTML("
        .shiny-output-error-validation2 {
        color: red; font-weight: normal;
        }
      ")),
      tags$style(HTML("
        .shiny-output-error-validation3 {
        color: blue; font-weight: normal;
        }
      "))
    ),
    # tags$head(
    #   tags$style(HTML("hr {border-top: 1px solid #D6D6D6;}"))
    # ),

    ### UI code separated by tabs
    tabItems(
      ui.roadmap(),      # eSDM Roadmap and Load or Save Workspace
      ui.loadModels(),   # Import Predictions
      ui.overlay(),      # Overlay Predictions
      ui.createEns(),    # Create Ensemble Predictions
      ui.evalMetrics(),  # Evaluation Metrics
      ui.prettyPlot(),   # High Quality Maps
      ui.export(),       # Export Predictions
      ui.manual()        # Manual
    )
  )
)


###############################################################################
##### SERVER


#------------------------------------------------------------------------------
### Naming convention
# File names: 'server/ui' + 'tab number' + camelCase + description
# File names (cont): '_' used as separator
# Separator for variable and function names: '_'
# Separator for variables that aren't being passed from server to ui: '.'
#   This includes reactiveValues
# renderUI output names for widgets: 'output$inputId_uiOut_(widget type)'
# Note: 'importing predictions' was originally 'loading models'


### Pre-server work

# Max file upload size is now 150MB
options(shiny.maxRequestSize = 150 * 1024^2)

# Use to perform sequential rather than concurrent validate checks
`%then%` <- shiny:::`%OR%`

# Server-wide CRS code
crs.ll <- st_crs(4326) # WGS 84

# Plotting variables
pal.esdm <- c(
  "#313695", "#4575b4", "#74add1", "#abd9e9", "#d1e5f0",
  "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"
)
# Use rev(RColorBrewer::brewer.pal(10, "Spectral")) for values plots
leg.perc.esdm <- c(
  "Lowest 60%", "35 - 40%", "30 - 35%", "25 - 30%", "20 - 25%",
  "15 - 20%", "10 - 15%", "5 - 10%", "2 - 5%", "Highest 2%"
)


#------------------------------------------------------------------------------
server <- function(input, output, session) {
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


  ### Source general and tab-specific server code

  # General server code
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


  # Roadmap
  source(file.path("server_other", "server_roadmap_download.R"), local = TRUE, chdir = TRUE)


  # Import predictions (previously called 'load model predictions')
  source(file.path("server_1_loadModels", "server_1_loadModels.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_csv.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_raster.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_renderUI.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_1_loadModels", "server_1_loadModels_shpgdb.R"), local = TRUE, chdir = TRUE)


  # Overlay predictions
  source(file.path("server_2_overlay", "server_2_overlay.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_csv.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_shpgdb.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_loadPoly_provided.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_overlayModels_base.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_2_overlay", "server_2_overlay_renderUI.R"), local = TRUE, chdir = TRUE)


  # Create ensemble predictions
  source(file.path("server_3_createEns", "server_3_createEns.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create_regexc.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_create_weighted.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_3_createEns", "server_3_createEns_renderUI.R"), local = TRUE, chdir = TRUE)


  # Calculate evaluation metrics
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_funcs.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_loadData.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_4_evalMetrics", "server_4_evalMetrics_renderUI.R"), local = TRUE, chdir = TRUE)


  # Make high quality maps (pretty plots)
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


  # Export predictions
  source(file.path("server_6_export", "server_6_export.R"), local = TRUE, chdir = TRUE)
  source(file.path("server_6_export", "server_6_export_renderUI.R"), local = TRUE, chdir = TRUE)


  # Manual
  # The function tags$iframe(...) is in ui.R so that the manual renders immediately
}


###############################################################################


shiny::shinyApp(ui = ui, server = server)
