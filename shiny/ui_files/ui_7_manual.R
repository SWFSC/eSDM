ui.manual <- function() {
  tabItem(tabName = "manual",
          fluidRow(
            column(12, 
                   fluidRow(
                     box(title = NULL, width = 12, 
                         uiOutput("manual_pdf")
                     )
                   )
            )
          )
  )
}