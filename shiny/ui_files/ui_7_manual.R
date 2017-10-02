### UI code for the 'Manual' tab

ui.manual <- function() {
  tabItem(
    tabName = "manual",
    fluidRow(
      column(
        width = 12, 
        fluidRow(
          box(
            width = 12, 
            fluidRow(
              column(
                width = 10, offset = 1, 
                tags$strong("Click the 'Fit to page' in the pdf viewer (above the '+' and '-' buttons)", 
                            "once or twice to resize the display of the manual.", tags$br(), 
                            "You may also click the 'Download' button to download the manual in as a PDF."), 
                tags$br(), 
                tags$br(), 
                tags$br(), 
                tags$iframe(height = "700px", width = "100%", scrolling = "yes",
                            src = "Ensemble_app_manual.pdf")
                # The above code could also be in server.R within a renderUI(), with corresponding uiOutput() here
              )
            )
          )
        )
      )
    )
  )
}