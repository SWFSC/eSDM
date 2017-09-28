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
            uiOutput("manual_pdf")
          )
        )
      )
    )
  )
}