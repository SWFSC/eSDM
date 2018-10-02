### UI code for the 'eSDM GUI Roadmap and Load or Save Workspace' tab

ui.roadmap <- function() {
  tabItem(
    tabName = "roadmap",
    fluidRow(
      column(
        width = 5,
        fluidRow(
          box(
            title = "Load or Save the GUI Workspace", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            tags$h5(tags$strong("GUI workspace definition:"),
                    "The 'GUI workspace' consists of data that has been imported into the GUI",
                    "(e.g. predictions, a study area polygon, or validation data) or created using the GUI",
                    "(e.g. overlaid or ensemble predictions, evaluation metrics, or saved high quality maps)",
                    "Thus, this data is saved in the downloaded '.RDATA' file and can be loaded back into the GUI.",
                    "However, user selections (e.g. high quality map parameters) and displayed plots",
                    "are not saved in the workspace and cannot be restored."),
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
              column(4, tags$img(src = "noaa_logo.png",  style = "height: 180px")),
              column(
                width = 7, offset = 1,
                tags$h5(
                  tags$strong("Credits"),
                  tags$p("This project was made possible by the Office of Science and Technology",
                         "as part of the National Protected Species Toolbox initiative.")
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
        width = 7,
        fluidRow(
          box(
            title = "eSDM GUI Roadmap", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            #------------------------------------------------------------------
            tags$h5(tags$strong("Overview:"),
                    "Ensemble tool for predictions from Species Distribution Models (eSDM) is an R package and a user-friendly spatial tool,",
                    "and includes this graphical user interface (GUI) which ensures that the tool is accessible to non-R users.",
                    "eSDM allows users to overlay SDM predictions onto a single base geometry, ",
                    "create ensembles of overlaid predictions via weighted or unweighted averages,",
                    "calculate performance metrics for each set of predictions and for resulting ensembles,",
                    "and visually compare predictions."),
            tags$h5("This 'roadmap' is intended to provide a brief overview of how you can use the various sections of the GUI,",
                    "and the order in which you can use them.",
                    "The roadmap is NOT intended to replace the manual, and you should refer to the manual for input format requirements",
                    "and other technical questions."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("1) Import SDM predictions or load workspace:"),
                    "You must first either import SDM predictions in the 'Import Model Predictions' tab,",
                    "or load a saved workspace from a previous GUI session in the",
                    tags$em("Load a saved GUI workspace"), "section.",
                    "You cannot use any of the other sections of the GUI until you perform one of these two steps."),
            tags$h5("Note: you can download a zip file with sample files (SDM predictions, study areas, validation data)",
                    "that you can use in the GUI by clicking the", tags$em("Download sample data"), "button below.",
                    "These sample data can be a useful reference if you're having trouble importing your own data."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("2) Overlay predictions:"),
                    "In the 'Overlay Model Predictions' tab, you can overlay imported SDM predictions (i.e. original predictions)",
                    "onto a single base geometry so all predictions have the same spatial resolution and coordinate system.",
                    "You can also import a study area polygon to clip the base geometry and/or",
                    "import an erasing polygon to erase area from the base geometry, ",
                    "e.g. to specify a study area or erase land from marine predictions."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("3) Create ensemble predictions:"),
                    "In the 'Create Ensemble Predictions' tab, you can rescale the overlaid predictions and",
                    "create ensemble predications via a weighted or unweighted average.",
                    "Weights can be the evaluation metrics of the overlaid predictions or",
                    "assigned by you to the overlaid predictions at several resolutions:",
                    "the entire study area, selected regions, or individual polygons within the overlaid predictions."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("4) Calculate metrics, plot maps, and export predictions:"),
                    "Within the 'Evaluation Metrics', 'High Quality Maps', and 'Export Predictions' tabs",
                    "you can import validation data and calculate evaluation metrics (AUC, TSS, RMSE), plot high quality maps,",
                    "and export predictions from the GUI, respectively.",
                    "Predictions can be exported to an Excel .csv file, a shapefile, or a KML/KMZ file."),
            tags$h5("Note that you only need imported (original) predictions to use these sections.",
                    "Thus, you can use the GUI for simply visualizing or evaluating predictions,",
                    "even if you have no need to create ensembles."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("5) Manual:"),
                    "In the 'Manual' tab, you can view and download the GUI manual.",
                    "The manual is divided into sections corresponding to the tabs and their boxes, and provides detailed information",
                    "about input format requirements and processes such as the overlay process.",
                    "Depending on the browser you are using,",
                    "the manual may automatically open in a separate window rather than being displayed within the GUI."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("Running the GUI locally:"),
                    "R Shiny applications such as the GUI can be hosted and used online;",
                    "however, running the R Shiny apps locally can be faster than running from them online.",
                    "Instructions for installing and running the eSDM GUI locally can be found at ",
                    tags$a("https://github.com/smwoodman/eSDM", href = "https://github.com/smwoodman/eSDM"),
                    "and in the 'Running the GUI locally' section of the GUI manual."),
            tags$br(),
            #------------------------------------------------------------------
            tags$strong("GUI tips:"),
            tags$ul(
              tags$li("The GUI can only perform a single operation at a time. Thus, if a process is running (e.g. the overlay process)",
                      "do not try to perform other actions (e.g. plotting a preview) as this may cause undefined behavior within the GUI."),
              tags$li("When a longer process is running, a progress bar will appear in the bottom right corner of the GUI.",
                      "This progress bar has an 'X' that closes the progress bar, but closing this bar will not stop the",
                      "currently running process."),
              tags$li("The larger the data sets, the longer all processes will take.",
                      "In addition, plotting will take longer when predictions span the antimeridian (i.e. decimal 180 degrees).")
            ),
            tags$br(),
            downloadButton("download_sample_data", "Download sample data"),
            downloadButton("download_manual", "Download manual")
          )
        )
      )
    )
  )
}
