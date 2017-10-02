### UI code for the 'etSDM Roadmap and Load or Save Session' tab

ui.roadmap <- function() {
  tabItem(
    tabName = "roadmap", 
    fluidRow(
      box(
        title = "Load and Save App Environment", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, 
        fluidRow(
          column(
            width = 6, 
            fileInput("load_app_envir_file", h5("Load saved app environment"), accept = ".RDATA"), 
            tags$span(textOutput("load_envir_text"), style = "color: blue")
          ), 
          column(
            width = 6, 
            textInput("save_app_envir_name", h5("Filename with which to save environment"), 
                      value = paste0("etSDM_", gsub("-", "", Sys.Date()), ".RDATA")), 
            downloadButton("save_app_envir", "Save current app environment")
          )
        )
      )
    ), 
    fluidRow(
      box(
        width = 6, 
        h4("TODO: Roadmap")
      )
    )
  )
}