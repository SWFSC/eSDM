### Sending user feedback to esdm.feedback@gmail.com

###############################################################################
### Generate warning message if no internet connection is detected
feedback_internet_connection <- reactive({
  input$tabs
  input$feedback_name
  input$feedback_email
  
  ifelse(isTruthy(try(getURL("www.google.com"), silent = TRUE)), 
         "",
         paste("Warning: no internet connection is detected.", 
               "You must be connected to the internet to submit feedback."))
})

### Send feedback and give user confirmation message
feedback_submit <- eventReactive(input$feedback_submit_event, {
  ### Process user inputs to prepare for email
  # User name and email
  f.name    <- paste("Name:", input$feedback_name)
  f.email   <- paste("Email:", input$feedback_email)
  
  # Selected tab(s)
  f.tab <- paste(ifelse(length(input$feedback_tab) > 1, "Tabs:", "Tab:"), 
                 paste(input$feedback_tab, collapse = "; "))
  
  # Actual comment
  f.comment <- unlist(strsplit(paste("Comments:",  input$feedback_comment), "\n"))
  
  # Email details
  from <- sprintf("<sendmailR@\\%s>", "eSDM")
  to <- "<esdm.feedback@gmail.com>"
  subject <- "eSDM feedback submission testing"
  body <- list(f.name, f.email, f.tab, "", f.comment)
  
  # Check that all field have filled
  validate(
    need(all(sapply(body, isTruthy)), 
         "Please provide an entry for all fields")
  )
  
  ### Send email
  mail.out <- try(suppressWarnings(sendmail(from, to, subject, body,
                                            control = list(smtpServer="ASPMX.L.GOOGLE.COM"))), 
                  silent = TRUE)
  
  validate(
    need(isTruthy(mail.out), 
         paste("Feedback could not be submitted; please check your internet", 
               "connection. If this problem persists, contact", 
               "Karin Forney (karin.forney@noaa.gov)."))
  )
  
  "Feedback submitted successfully. Thank you!"
})

###############################################################################
