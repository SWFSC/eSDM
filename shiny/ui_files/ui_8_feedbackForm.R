### UI code for the 'Submit Feedback' tab

ui.feedbackForm <- function() {
  tabItem(
    tabName = "feedbackForm",
    fluidRow(
      box(
        title = "Feedback Form", status = "warning", solidHeader = FALSE, width = 6, 
        helpText(tags$strong("All fields are required")), 
        tags$span(textOutput("feedback_internet_connection_text"), style = "color: red"), 
        fluidRow(
          column(6, textInput("feedback_name", tags$h5("Name"), value = "")), 
          column(6, textInput("feedback_email", tags$h5("Email"), value = "")) 
        ), 
        selectizeInput("feedback_tab", tags$h5("Tab(s) relating to comment"), width = "50%", 
                       choices = list("General comment", "eSDM Roadmap and Load or Save Session", 
                                      "Load Model Predictions", "Overlay Model Predictions", 
                                      "Create Ensemble Predictions", "Evaluation Metrics", "High Quality Maps", 
                                      "Export Predictions", "Manual"), 
                       selected = NULL, multiple = TRUE), 
        # selectizeInput("feedback_tab", tags$h5("Tab(s) relating to comment"), width = "50%", 
        #                choices = list("General comment" = 1, "eSDM Roadmap and Load or Save Session" = 2, 
        #                               "Load Model Predictions" = 3, "Overlay Model Predictions" = 4, 
        #                               "Create Ensemble Predictions" = 5, "Evaluation Metrics" = 6, "High Quality Maps" = 7, 
        #                               "Export Predictions" = 8, "Manual" = 9), 
        #                selected = NULL, multiple = TRUE), 
        textAreaInput("feedback_comment", tags$h5("Comments. Please be as specific as possible."), value = "", 
                      rows = 5, resize = "vertical"),
        actionButton("feedback_submit_event", "Submit feedback"), 
        textOutput("feedback_submit_text")
      )
    )
  )
}