ui.feedbackForm <- function() {
  tabItem(tabName = "feedbackForm",
          fluidRow(
            box(
              title = "Feedback Form", status = "warning", solidHeader = FALSE, width = 6, 
              helpText(strong("All fields are required")), 
              fluidRow(
                column(6, textInput("feedback_name", h5("Your name"), value = "")), 
                column(6, textInput("feedback_email", h5("Your email"), value = "")) 
              ), 
              selectInput("feedback_tab", h5("Tab relating to comment"), width = "50%", 
                          choices = list("General" = 1, "Load Model Predictions" = 2, "Overlay Model Predictions" = 3, 
                                         "Create Ensemble Predictions" = 4, "Evaluation Metrics" = 5, "High Quality Maps" = 6, 
                                         "Export Predictions" = 7, "Manual" = 8), 
                          selected = 1), 
              textAreaInput("feedback_comment", h5("Your comments. Please be as specific as possible"), value = "", 
                            rows = 5, resize = "vertical"),
              actionButton("feedback_submit", "Submit feedback")
            )
          )
  )
}