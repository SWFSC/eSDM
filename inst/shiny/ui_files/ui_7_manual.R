### UI code for the 'Manual' tab

ui.manual <- function() {
  tabItem(
    tabName = "manual",
    fluidRow(
      column(
        width = 10, offset = 1,
        fluidRow(
          box(
            title = "eSDM GUI Manual", width = 12,
            tags$strong("Click the 'Fit to page' button in the pdf viewer (above the '+' and '-' buttons)",
                        "once or twice to resize the display of the manual to fit to page and fit to width, respectively.",
                        tags$br(),
                        "You also can click the 'Download' button on the top bar of the pdf viewer",
                        "to download the manual as a PDF."),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$iframe(height = "700px", width = "100%", scrolling = "yes", src = "eSDM_manual.pdf")
            # The above code could also be in server.R within a renderUI(), with corresponding uiOutput() here
          )
        )
      )
    )
  )
}
