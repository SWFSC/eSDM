### ui.R for Ensemble tool for predictions from Species Distribution Models (eSDM) GUI
# Author: Sam Woodman


###############################################################################
# Naming convention, commonly used ui objects, and tab-specific ui scripts

###########################################################
### Naming convention
# File names: 'server/ui' + 'tab number' + camelCase + description
# File names (cont): '_' used as separator
# Separator for variable and reactive function names: '_'
# Separator for variables that aren't being passed from server to ui: '.'
#    This includes reactiveValues
# Separator for non-reactive functions: '.'
# renderUI output names for widgets: 'output$inputId_uiOut_(widget type)'


###############################################################################
### Load packages needed by ui code
library(DT)
library(leaflet)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)


###########################################################
### ui code (lists, instructions, etc) used in multiple tabs
source(file.path("ui_files", "ui_common.R"), local = TRUE, echo = FALSE, chdir = TRUE)

### ui code parsed by tabName
source(file.path("ui_files", "ui_0_roadmap.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_1_loadModels.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_2_overlay.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_3_createEns.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_4_evalMetrics.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_5_prettyPlot.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_6_export.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_7_manual.R"), local = TRUE, echo = FALSE, chdir = TRUE)


###############################################################################
### UI object for Shiny app
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
      menuItem("Import Model Predictions", tabName = "loadModels", icon = icon("cloud-upload")),
      menuItem("Overlay Model Predictions", tabName = "overlay", icon = icon("cogs")),
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
    # extendShinyjs(text = jscode, functions = c("closeWindow")),

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
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #D6D6D6;}"))
    ),

    ### UI code separated by tabs
    tabItems(
      ui.roadmap(),      # eSDM Roadmap and Load or Save Session
      ui.loadModels(),   # Import Model Predictions
      ui.overlay(),      # Overlay Model Predictions
      ui.createEns(),    # Create Ensemble
      ui.evalMetrics(),  # Evaluation Metrics
      ui.prettyPlot(),   # High Quality Maps
      ui.export(),       # Export Predictions
      ui.manual()        # Manual
    )
  )
)
