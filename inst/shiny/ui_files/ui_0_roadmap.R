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
                    "(e.g., predictions or validation data) or created using the GUI",
                    "(e.g. overlaid or ensemble predictions, evaluation metrics, or saved high quality maps)",
                    "Thus, this data is saved in the downloaded '.RDATA' file and can be loaded back into the GUI.",
                    "However, user selections (e.g., high quality map parameters) and displayed plots",
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
                textInput("save_app_envir_name", tags$h5("Filename (without file extension)"),
                          value = paste0("eSDM_", gsub("-", "", Sys.Date()))),
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
                  tags$p("This project was made possible by the NOAA Fisheries Office of Science and Technology",
                         "as part of the National Protected Species Toolbox initiative.")
                ),
                tags$h5(
                  tags$strong("Disclaimer"),
                  tags$p("This application is developed and maintained by scientists at the",
                         "NOAA Fisheries Southwest Fisheries Science Center and should not be",
                         "construed as official communication of NMFS, NOAA, or the U.S. Dept. of Commerce.",
                         "While the best efforts have been made to ensure the highest quality,",
                         "tools such as this are under constant development and are subject to change.")
                )
              )
            )
          ),
          box(
            width = 12,
            tags$h5(
              tags$strong("Citation"),
              tags$p("Woodman, S.M., Forney, K.A., Becker, E.A., DeAngelis, M.L., Hazen, E.L., Palacios, D.M., Redfern, J.V.",
                     "(2019). eSDM: A tool for creating and exploring ensembles of predictions",
                     "from species distribution and abundance models.",
                     tags$em("Methods Ecol Evol."), "2019;00:1-11. doi:10.1111/2041-210X.13283"),
              tags$a("Woodman et al. 2019 - PDF", href = "https://doi.org/10.1111/2041-210X.13283")
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
                    "Ensemble tool for predictions from Species Distribution Models (eSDM) is a user-friendly spatial tool",
                    "that includes this graphical user interface (GUI), making eSDM accessible to non-R users.",
                    "eSDM allows users to overlay SDM predictions onto a single base geometry, ",
                    "create ensembles of overlaid predictions and their associated uncertainty",
                    "via weighted or unweighted averages,",
                    "calculate performance metrics for each set of predictions and for resulting ensembles,",
                    "and visually compare predictions."),
            tags$h5("This roadmap provides a brief overview of the various sections of the GUI,",
                    "as well as the order in which you can use these sections",
                    "The roadmap is NOT intended to replace the GUI manual. You should refer to the manual for",
                    "technical details, such as format requirements of input files."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("1) Import predictions or load workspace:"),
                    "To begin your session, you can either import SDM predictions",
                    "(including their associated uncertainty) in the 'Import Predictions' tab,",
                    "or load a saved workspace from a previous GUI session in the",
                    tags$em("Load a saved GUI workspace"), "section of this tab.",
                    "You cannot use any of the other sections of the GUI until you perform one of these two steps."),
            tags$h5("Note: you can download zip files with sample data (SDM predictions, regional polygons, and validation data)",
                    "or the data used in the example analysis of Woodman et al. (2019)",
                    "by clicking the", tags$em("Download sample data"), "or the",
                    tags$em("Download manuscript data"), "buttons below, respectively.",
                    "These data can be a useful reference for data format requirements."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("2) Overlay predictions:"),
                    "In the 'Overlay Predictions' tab, you can overlay imported SDM predictions (i.e., original predictions)",
                    "onto a base geometry so all predictions have the same spatial resolution and coordinate system.",
                    "You can also import a study area polygon and/or an erasing polygon to",
                    "clip or erase area from the base geometry, respectively",
                    "(e.g., to specify a study area or erase land from marine predictions.)"),
            #------------------------------------------------------------------
            tags$h5(tags$strong("3) Create ensemble predictions:"),
                    "After overlaying the predictions, you can rescale the overlaid predictions and",
                    "create ensemble predications via a weighted or unweighted average in the 'Create Ensemble Predictions' tab.",
                    "Weights can be based on the evaluation metrics of the overlaid predictions,",
                    "the inverse of the variance of the overlaid predictions, or assigned by users",
                    "either for the entire study area or for each prediction polygon.",
                    "Each set of ensemble predictions will have associated uncertainty values, calculated",
                    "using either the user-specified prediction (within-model) uncertainty  or the among-model uncertainty"),
            #------------------------------------------------------------------
            tags$h5(tags$strong("4) Calculate metrics, plot maps, and export predictions:"),
                    "Within the 'Evaluation Metrics', 'High Quality Maps', and 'Export Predictions' tabs",
                    "you can import validation data and calculate evaluation metrics (AUC, TSS, RMSE), plot high quality maps,",
                    "and export predictions from the GUI, respectively.",
                    "Predictions can be exported to a CSV file, a shapefile, or a KML/KMZ file."),
            tags$h5("Note that you only need imported (original) predictions to use these sections.",
                    "Thus, you can use the GUI for simply visualizing or evaluating predictions,",
                    "even if you have no need to create ensembles."),
            #------------------------------------------------------------------
            tags$h5(tags$strong("5) Manual:"),
                    "In the 'Manual' tab, you can view and download the GUI manual.",
                    "The manual is divided into sections corresponding to the tabs and their sections,",
                    "and provides detailed information",
                    "about input format requirements and processes such as the overlay process.",
                    "Depending on the browser you are using and your browser settings,",
                    "the manual may automatically open in a separate window rather than being displayed within the GUI.",
                    "You can also download the manual by clicking the", tags$em("Download manual"), "button below."),
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
              tags$li("If text or images overlap, please adjust the text size in your browser ",
                      "(e.g., Ctrl - minus ('-') on Windows systems)"),
              tags$li("When a longer process is running one or both of the following will happen:",
                      "a pulsing dot will appear in the top right corner of the window,",
                      "or a progress bar will appear in the bottom right corner of the GUI.",
                      "Clicking the 'X' on the progress bar closes the bar,",
                      "but does not stop the currently running process."),
              tags$li("The GUI can only perform a single operation at a time. Thus, if a process",
                      "(e.g., the overlay process) is running, do not try to perform other actions,",
                      "as this may cause undefined behavior within the GUI."),
              tags$li("The larger the data sets, the longer all processes will take.",
                      "In particular, plotting will take longer when predictions span the antimeridian (i.e., 180 decimal degrees)."),
              tags$li("If your download (e.g., of manuscript data) is a 'download.htm' file, cancel and try the download again.")
            ),
            tags$br(),
            tags$h5("Submit suggestions and bug reports at",
                    tags$a("https://github.com/smwoodman/eSDM/issues,", href = "https://github.com/smwoodman/eSDM/issues"),
                    "or contact Sam Woodman (sam.woodman@noaa.gov) with any questions"),
            tags$br(),
            downloadButton("download_data_manuscript", "Download manuscript data"),
            downloadButton("download_data_sample", "Download sample data"),
            downloadButton("download_manual", "Download manual")
          )
        )
      )
    )
  )
}
