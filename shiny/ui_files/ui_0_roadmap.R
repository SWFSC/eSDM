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
                         "be used for official decisions with out consultation.", 
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
            width = 12, 
            h4("eSDM Roadmap")
            # downloadButton("download_sample_data", "Download sample data")
          )
        )
      )
    )
  )
}