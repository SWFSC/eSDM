ui.manual <- function() {
  tabItem(tabName = "manual",
          fluidRow(
            column(8, offset = 2, 
                   fluidRow(
                     box(title = NULL, width = 12, height = 800, 
                         uiOutput("manual_pdf")
                     )
                   )
            )
          )
  )
}