### ui.R 
## For Ensemble Shiny App
# Designed by Sam Woodman


###############################################################################
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT) # Load here too because we need DT::dataTableOutput
library(shinycssloaders)


###############################################################################
# Naming convention, tab-specific ui scripts, and commonly used objects

###########################################################
### Naming convention
# File names are 'server' or 'ui' _ tab number _ tab camelCase _ details
# Use '_' as separator for reactive function names
# Use '.' as separator for variables that aren't passed from server to ui, 
#     including reactiveValues, 
# First part of reactiveValue variables name is tab code of where value was set
# renderUI output$... names are: 'inputId + _uiOut_(type of widget)'


###########################################################
### ui code parsed by tabName
source(file.path("ui_files", "ui_1_loadModels.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_2_overlay.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_3_createEns.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_4_evalMetrics.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_5_prettyPlot.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_6_export.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_7_manual.R"), local = TRUE, echo = FALSE, chdir = TRUE)
source(file.path("ui_files", "ui_8_feedbackForm.R"), local = TRUE, echo = FALSE, chdir = TRUE)

### ui code (lists, instructions, etc) used in multiple tabs
source(file.path("ui_files", "ui_common.R"), local = TRUE, echo = FALSE, chdir = TRUE)


###############################################################################
### UI object for Shiny app
ui <- dashboardPage(
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
    # hand-rock-o hand-paper-o hand-scissors-o 
    # hand-lizard-o hand-spock-o hand-peace-o money
    br(), 
    actionButton("close_app", label = "Close App"), 
    
    ###################################### TEMP Start
    ui.new.line(),
    ui.new.line(), 
    actionButton("load_all", "Load default"),
    actionButton("save_all", "Save current list.all") 
    ###################################### TEMP End
  ),
  
  dashboardBody(
    useShinyjs(),        # Required by shinyjs package
    
    tabItems(
      ui.loadModels(),   # Load Model Predictions
      ui.overlay(),      # Overlay Model Predictions
      ui.createEns(),    # Create Ensemble tab
      ui.evalMetrics(),  # Evaluation Metrics tab
      ui.prettyPlot(),   # High Quality Maps tab
      ui.export(),       # High Quality Maps tab
      ui.manual(),       # Manual tab
      ui.feedbackForm()  # Feedback form
    )
  )
)