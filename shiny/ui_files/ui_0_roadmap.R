### UI code for the 'eSDM Roadmap and Load or Save Environment' tab

ui.roadmap <- function() {
  tabItem(
    tabName = "roadmap", 
    fluidRow(
      column(
        width = 6, 
        fluidRow(
          box(
            title = "Load and Save eSDM Working Environment", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
            fluidRow(
              box(
                width = 6, 
                tags$strong("Load eSDM working environment"), 
                fileInput("load_app_envir_file", tags$h5("Upload .RDATA file"), accept = ".RDATA"), 
                tags$span(textOutput("load_envir_text"), style = "color: blue")
              ), 
              box(
                width = 6, 
                tags$strong("Save eSDM working environment"), 
                textInput("save_app_envir_name", tags$h5("Filename for saved eSDM working environment"), 
                          value = paste0("eSDM_", gsub("-", "", Sys.Date()), ".RDATA")), 
                downloadButton("save_app_envir", "Download current eSDM working environment")
              )
            )
          ), 
          box(
            width = 12, #background = "light-blue", #title = "Disclaimer and credits"
            # Valid colors are: shinydashboard::validColors()
            fluidRow(
              column(3, tags$img(src = "noaa_logo.png",  style = "height: 170px")), 
              column(
                width = 8, offset = 1, 
                tags$h5(
                  tags$strong("Credits"), 
                  tags$p("Development of eSDM was made possible by NOAA's Office of Science and Technology", 
                         "and Southwest Fisheries Science Center.")
                ), 
                tags$h5(
                  tags$strong("Disclaimer"), 
                  tags$p("This application is developed and maintained by scientists at the", 
                         "NOAA Fisheries Southwest Fisheries Science Center and should not be", 
                         "construed as official communication of NMFS, NOAA, or the U.S. Dept. of Commerce.", 
                         "This is the beta version of this application, and should not be cited nor should the information", 
                         "be used for official decisions without consultation.", 
                         "While the best efforts have been made to insure the highest quality,", 
                         "tools such as this are under constant development and are subject to change.")
                )
              )
            )
          )
        )
      ), 
      column(
        width = 6, 
        fluidRow(
          box(
            title = "eSDM Roadmap", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
            tags$h5(tags$strong("Overview:"), 
                    "The eSDM is a user-friendly spatial tool with a web-based interface that allows users to import", 
                    "spatial density model (SDM) layers and create and explore ensemble predictions to inform management", 
                    "and explore spatial uncertainties.", 
                    "This section is intended to provide you a roadmap that gives a brief overview of the eSDM and the order", 
                    "in which you should use the sections of the app.", 
                    "This section is NOT intended to explain inputs or replace the manual in any fashion."), 
            tags$h5(tags$strong("1) Load SDM predictions:"), 
                    "The main function of the eSDM is to load in SDM predictions, overlay them onto the same grid,", 
                    "and then create ensemble predictions.", 
                    "Thus, the first step is to either load SDM predictions in the 'Load Model Predictions' tab", 
                    "or load a saved working environment from a previous eSDM session in the", 
                    tags$em("Load eSDM working environment"), "section.", 
                    "You cannot use any of the other functionality in the app until you perform one of these two steps."), 
            tags$h5("If you do not have any SDM predictions on hand but still want to use the eSDM,", 
                    "you can download a zip file with sample model predictions that you can use in the eSDM by", 
                    "clicking the", tags$em("Download sample data"), "button below."), 
            tags$h5(tags$strong("2) Overlay and create ensembles:"), 
                    "Next, you can overlay your loaded SDM predictions in the 'Overlay Model Predictions' tab.", 
                    "Note that you should use the 'same-grid' overlay process if all of your loaded SDM predictions are already on the", 
                    "same grid. This will give you the same result as using the 'standard' overlay process, but in much less time.", 
                    "After creating the overlaid predictions, you can move on to the", 
                    "'Create Ensemble Predictions' tab to create your ensemble predictions."), 
            tags$h5(tags$strong("3) Evaluate, produce maps of, and export predictions:"), 
                    "Once you have loaded at least one set of SDM predictions into the eSDM,", 
                    "you can use the 'Evaluation Metrics', 'High Quality Maps', and 'Export Predictions' tabs to", 
                    "calculate evaluation metrics using validation data, produce high quality maps, or export any of the", 
                    "loaded predictions or predictions created with the eSDM, respectively."), 
            tags$h5(tags$strong("4) Manual and eSDM feedback:"), 
                    "You can view and/or download the eSDM manual in the 'Manual' tab.", 
                    "The manual is divided into sections corresponding to the tabs and their boxes, and provides detailed information", 
                    "about the input format requirements and what exactly is happening during certain steps such as the overlay process.", 
                    "Depending on the browser you are using,", 
                    "the manual may automatically open in a separate window rather than being displayed within the eSDM."), 
            tags$h5("The eSDM is a prototype and still under development. If you encounter an error while using your data with the app", 
                    "or have comments on any facet of the eSDM, please use the 'Submit Feedback' tab to submit your feedback so that", 
                    "we can address your concerns in future versions of the eSDM."), 
            tags$h5(tags$strong("Run the eSDM locally:"), 
                    "One of the benefits of R Shiny applications such as the eSDM is that they can be hosted online so that users", 
                    "do not have to run them through R themselves.", 
                    "However, running the R Shiny apps locally can be faster than running from them from a server.", 
                    "Thus, you can download the eSDM code from GitHub", 
                    tags$a("at this link", href = "https://github.com/smwoodman/eSDM"), 
                    "and run the eSDM locally. See the 'Run the eSDM locally' section of the manual for instructions", 
                    "on how to run the eSDM locally."), 
            tags$br(), 
            tags$br(), 
            downloadButton("download_sample_data", "Download sample data")
          )
        )
      )
    )
  )
}