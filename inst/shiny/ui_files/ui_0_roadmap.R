### UI code for the 'eSDM GUI Roadmap and Load or Save Workspace' tab

ui.roadmap <- function() {
  tabItem(
    tabName = "roadmap",
    fluidRow(
      column(
        width = 6,
        fluidRow(
          box(
            title = "Load and Save GUI Workspace", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            fluidRow(
              box(
                width = 6,
                tags$strong("Load a saved GUI workspace"),
                fileInput("load_app_envir_file", tags$h5("Upload .RDATA file"), accept = ".RDATA"),
                tags$span(textOutput("load_envir_text"), style = "color: blue")
              ),
              box(
                width = 6,
                tags$strong("Save current GUI workspace"),
                textInput("save_app_envir_name", tags$h5("Filename (must have '.RDATA' extension)"),
                          value = paste0("eSDM_", gsub("-", "", Sys.Date()), ".RDATA")),
                downloadButton("save_app_envir", "Download GUI workspace")
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
                         "This is a developmental version of this application, and results generated using this app",
                         "should not be cited nor should the information be used for official decisions without consultation.",
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
            title = "eSDM GUI Roadmap", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            tags$h4("TODO - proof"),
            tags$h5(tags$strong("Overview:"),
                    "Ensemble tool for predictions from Species Distribution Models (eSDM) is a user-friendly spatial tool",
                    "with this graphical user interface (GUI) that allows users to overlay SDM predictions onto a single base geometry,",
                    "create ensembles of these predictions via weighted or unweighted averages,",
                    "calculate performance metrics for each set of predictions and for resulting ensembles,",
                    "and visually compare ensemble predictions with original predictions",
                    "This section is intended to provide you a roadmap that gives a brief overview of the eSDM and the order",
                    "in which you can use the sections of the app.",
                    "This section is NOT intended to explain inputs or replace the manual in any fashion."),
            tags$h5(tags$strong("1) Import SDM predictions or load workspace:"),
                    "The main function of the eSDM is to import SDM predictions, overlay the predictions onto the same geometry,",
                    "and use the overlaid predictions create ensemble predictions.",
                    "Thus, the first step is to either import SDM predictions in the 'Import Model Predictions' tab",
                    "or load a saved workspace from a previous GUI session in the",
                    tags$em("Load a saved GUI workspace"), "section.",
                    "You cannot use any of the other functionality in the GUI until you perform one of these two steps."),
            tags$h5("Note: you can download a zip file with sample files (SDM predictions, study areas, validation data)",
                    "that you can use in the GUI by clicking the", tags$em("Download sample data"), "button below.",
                    "These sample data are a good place to start if you're having trouble importing your own data."),
            tags$h5(tags$strong("2) Overlay predictions and create ensembles:"),
                    "Next, you can overlay your loaded SDM predictions onto a chosen base geometry in the 'Overlay Model Predictions' tab.",
                    "You can specify a study area by loading a study area polygon and/or load an erasing polygon to crop area from",
                    "the predictions, e.g. if you are working with predictions for a marine environment and wish to crop land area.",
                    "After creating the overlaid predictions, you can create ensemble predications in the",
                    "'Create Ensemble Predictions' tab."),
            tags$h5(tags$strong("3) Calculate metrics, generate maps, and export predictions:"),
                    "You can use the 'Evaluation Metrics', 'High Quality Maps', and 'Export Predictions' tabs",
                    "to calculate evaluation metrics (AUC, TSS, RMSE) using validation data, produce high quality maps,",
                    "and export predictions from the GUI, respectively.",
                    "Note that you only need original, imported predictions to use these sections.",
                    "This makes the GUI an applicable tool for simply visualizing or evaluating predictions,",
                    "even if you have no need to create ensembles."),
            tags$h5(tags$strong("4) Manual:"),
                    "You can view and/or download the GUI manual in the 'Manual' tab.",
                    "The manual is divided into sections corresponding to the tabs and their boxes, and provides detailed information",
                    "about the input format requirements and details about processes such as the overlay process.",
                    "Depending on the browser you are using,",
                    "the manual may automatically open in a separate window rather than being displayed within the GUI."),
            tags$h5(tags$strong("Run the GUI locally:"),
                    "R Shiny applications such as the GUI can be hosted online so that users",
                    "do not have to run them through R themselves.",
                    "However, running the R Shiny apps locally can be faster than running from them from a server.",
                    "Thus, you can install the R package", tags$em("eSDM"), "from ", #TODO CRAN
                    tags$a("https://github.com/smwoodman/eSDM", href = "https://github.com/smwoodman/eSDM"),
                    "and follow the instructions to run the GUI locally.",
                    "See the GitHub page and the 'Run the GUI locally' section of the manual for",
                    "additional details and instructions."),
            tags$br(),
            tags$br(),
            downloadButton("download_sample_data", "Download sample data")
          )
        )
      )
    )
  )
}
