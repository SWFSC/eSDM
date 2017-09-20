### ui.R for Ensemble Shiny App
# Designed by Sam Woodman


###############################################################################
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT) # Load here too because we need DT::dataTableOutput
library(shinycssloaders)


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


###########################################################
### ui code (lists, instructions, etc) used in multiple tabs
source(file.path("ui_files", "ui_common.R"), local = TRUE, echo = FALSE, chdir = TRUE)

### ui code parsed by tabName
source(file.path("ui_files", "ui_1_loadModels.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_2_overlay.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_3_createEns.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_4_evalMetrics.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_5_prettyPlot.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_6_export.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_7_manual.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_8_feedbackForm.R"), local = TRUE, echo = FALSE, chdir = TRUE)


###############################################################################
### UI object for Shiny app
ui <- dashboardPage(
  skin = "blue", 
  dashboardHeader(title = "Ensemble App"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs", 
                menuItem("Load Model Predictions", tabName = "loadModels", 
                         icon = icon("cloud-upload")), 
                menuItem("Overlay Model Predictions", tabName = "overlay", 
                         icon = icon("cogs")), 
                menuItem("Create Ensemble Predictions", tabName = "createEns", 
                         icon = icon("cog")), 
                menuItem("Evaluation Metrics", tabName = "evalMetrics", 
                         icon = icon("check")),
                menuItem("High Quality Maps", tabName = "prettyPlot", 
                         icon = icon("file-image-o")),
                menuItem("Export Predictions", tabName = "export", 
                         icon = icon("cloud-download")), 
                menuItem("Manual", tabName = "manual", 
                         icon = icon("book")), 
                menuItem("Submit feedback", tabName = "feedbackForm", 
                         icon = icon("commenting"))
    ),
    br(), 
    actionButton("close_app", label = "Close App")
  ),
  
  dashboardBody(
    useShinyjs(),       # Required by shinyjs package
    
    tabItems(           # Tab name:
      ui.loadModels(),   # Load Model Predictions
      ui.overlay(),      # Overlay Model Predictions
      ui.createEns(),    # Create Ensemble
      ui.evalMetrics(),  # Evaluation Metrics
      ui.prettyPlot(),   # High Quality Maps
      ui.export(),       # Export Predictions
      ui.manual(),       # Manual
      ui.feedbackForm()  # Feedback form
    )
  )
)